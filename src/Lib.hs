{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run, runWithCtx, mkPkgLookupCtx, mkAndPopulateStackDb
    ) where

import           Control.Arrow   (first, second)
import           Control.Monad   (foldM)
import           Data.Bifunctor  (Bifunctor (bimap))
import           Data.Foldable   (foldlM, toList)
import           Data.List       (find)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Set        as Set
import           Data.Text       (Text, pack, unpack)
import qualified Data.Text       as TxT
import qualified GhcPkg          as GPKG
import qualified LUtil           as Util
import qualified Packages        as PKG
import qualified Parser          as P
import           System.Exit     (ExitCode (..))
import qualified System.Process  as SP
import qualified Text.Megaparsec as MP
import qualified Types           as T
import Data.List (sortOn)
import Debug.Trace (traceShow, traceShowId)

newtype GhcPkgDbSource db = GhcPkgDbSource db

data ProcessedNode = PRawLine Text | POldImportCmt Text | PImportGroup T.PackageInfo [T.ImportDecl] deriving (Show)
type ProcessedAST = [ProcessedNode]

instance (GPKG.Database db) => PKG.MappingSource (GhcPkgDbSource db) where
    providerOfModule (GhcPkgDbSource db) name = pure $
            maybe (Left "could not find package") Right $ T.pkgInfo <$> GPKG.lookup db name

run :: T.SourceInfo Text -> IO (Text, Text)
run src = do
    ctx <- mkPkgLookupCtx
    runWithCtx ctx src

linesPreserve ""  = []
linesPreserve txt = TxT.split (=='\n') txt

unlinesPreserve []     = ""
unlinesPreserve [x]    = x
unlinesPreserve (x:"":xs) = x <> "\n" <> unlinesPreserve xs
unlinesPreserve (x:xs) = x <> "\n" <> unlinesPreserve xs

cmtToTxt (T.SingleLineCmt c) = "-- " <> c

unAST :: ProcessedAST -> Text
unAST = unlinesPreserve . map unpackAST . stripOld
    where
        unpackAST (PRawLine l) = l
        unpackAST (PImportGroup pkg imports) = unlinesPreserve $ cmtToTxt (packageToComment pkg):map snd imports
        stripOld = filter (\case
                            POldImportCmt _ -> False
                            _ -> True
                            )

runWithCtx :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> IO (Text, Text)
runWithCtx mPkgCtx (T.SourceInfo name content) = do
    case mPkgCtx of
        Left err -> pure (mempty, err)
        Right pkgCtx ->
            case MP.parse P.parseFile (unpack name) content of
            Left err -> pure (content, (("parse error: " <>) . pack . MP.errorBundlePretty) err)
            Right rs -> bimap unAST (unpackErrs . map (second (fmap fst))) <$> modifyContent pkgCtx content rs
    where
      unlinesNoTrailing _ [] = ""
      unlinesNoTrailing _ [x] = x
      unlinesNoTrailing orig lines
            | TxT.last orig /= '\n' = TxT.dropEnd 1 (TxT.unlines (tail lines)) <> last lines
            | otherwise = TxT.unlines lines
      unpackErrs [] = ""
      unpackErrs xs = "could not find packages for:" <> listPre <> prettyErrs xs
      prettyErrs xs = foldl (\xs x -> xs <> unpackErr x) mempty $ map (second T.unLocated) xs
      unpackErr (txt, name) = listPre <> T.modName name <> ": " <> txt
      listPre = "\n  - "


eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx

mkAndPopulateStackDb :: IO (T.Result GPKG.MapStore)
mkAndPopulateStackDb = GPKG.mkDbAndPopulate proc
    where
        proc = GPKG.pkgCmd (\(cmd, args) -> SP.proc "stack" (["exec", "--", cmd] ++ args))

mkPkgLookupCtx :: IO (T.Result (PKG.LocalPkgMatcher (GhcPkgDbSource GPKG.MapStore)))
mkPkgLookupCtx = do
    db <- ctx
    pure $ PKG.mkLocalMatcher <$> db
    where
        ctx = do
            db <- mkAndPopulateStackDb
            pure $ GhcPkgDbSource <$> db

packageCommentPrefix = "-- "

packageToComment :: T.PackageInfo -> T.Comment
packageToComment pkg = T.SingleLineCmt $ T.pkgName pkg

-- Second result is errors
extractImports ::
    PKG.MappingSource s => s
    -> T.Module
    -> IO (Map T.PackageInfo [T.Located T.ImportDecl], [(Text, T.Located T.ImportDecl)])
extractImports ctx mod = foldlM  doFold (mempty, mempty) (T.modImports mod)
    where
        doFold (xs, errs) name = do
            minfo <- PKG.providerOfModule ctx (fst $ T.unLocated name)
            pure $ case minfo of
                Left err -> (xs, (err, name):errs)
                Right info -> (M.insert info (name:M.findWithDefault [] info xs) xs, errs)

modifyContent :: PKG.MappingSource s => s -> Text -> T.Module -> IO (ProcessedAST, [(Text, T.Located T.ImportDecl)])
modifyContent mapCtx txt mod = do
    imports <- extractImports mapCtx mod
    let linesOut = Util.foldlWithIndex (foldLines (fst imports))
            mempty
            lines
    pure (linesOut, snd imports)
    where
        lines = linesPreserve txt
        txtForImport imp = lines !! ((T.srcLine $ T.posOf imp) - 1)
        importOnLine line = isJust $ find (==line) importLines
        commentOnLine line = isJust $ find (==line) $ map (T.srcLine . T.posOf) (T.modComments mod)
        linesForComment = filter (not . commentOnLine . (\x -> x - 1)) importLines
        importLines = concatMap Util.linesCoveredByImport $ T.modImports mod
        importsRange = case importLines of
            []    -> Nothing
            lines -> Just (minimum lines, maximum lines)
        toCommentGroup (pkg, imports) = PImportGroup pkg (map T.unLocated imports)
        oldComments pkgs = filter (`Set.notMember` pkgCmts pkgs) . Set.toList . pkgCmts . M.keysSet
            where
                pkgCmts = Set.map (cmtToTxt . packageToComment)
        commented :: Map T.PackageInfo [T.Located T.ImportDecl] -> ProcessedAST
        commented = map toCommentGroup . M.toList
        foldLines :: Map T.PackageInfo [T.Located T.ImportDecl] -> [ProcessedNode] -> Int -> Text -> [ProcessedNode]
        foldLines imports result lineNb line
            | maybe False ((lineNb ==) . fst) importsRange =
                result <> commented imports

            | commentOnLine lineNb && notPassthroughComment line = result <> [POldImportCmt line]
            | importOnLine lineNb = result
            | otherwise = nextV
            where
                nextV = result <> [PRawLine line]
                allPackages = Set.map packageToComment $ M.keysSet imports
                notPassthroughComment line = Set.member line (Set.map cmtToTxt allPackages)













