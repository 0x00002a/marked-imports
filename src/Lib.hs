{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( run, runWithCtx, mkPkgLookupCtx, mkAndPopulateStackDb, parseToAST,
    unAST, stripPackageComments, runT, sortImportsOn
    ) where

import           Control.Arrow   (first, second)
import           Control.Monad   (foldM, join)
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
import Control.Monad.Cont (liftIO)

newtype GhcPkgDbSource db = GhcPkgDbSource db

data ProcessedNode = PRawLine (T.Located Text) | POldImportCmt (T.Located Text) | PImportGroup T.PackageInfo [T.Located T.ImportDecl] deriving (Show, Eq)
type ProcessedAST = [ProcessedNode]

type ProcessErr = (Text, T.Located T.ImportDecl)

instance (GPKG.Database db) => PKG.MappingSource (GhcPkgDbSource db) where
    providerOfModule (GhcPkgDbSource db) name = pure $
            maybe (Left "could not find package") Right $ T.pkgInfo <$> GPKG.lookup db name

run :: T.SourceInfo Text -> IO (Text, Text)
run src = runT src id

runT :: T.SourceInfo Text -> (ProcessedAST -> ProcessedAST) -> IO (Text, Text)
runT src t = do
    ctx <- mkPkgLookupCtx
    runWithCtxT ctx src t

parseToAST :: PKG.MappingSource s => s -> T.SourceInfo Text -> IO (T.Result (ProcessedAST, [ProcessErr]))
parseToAST pkgCtx (T.SourceInfo name content) = do
    case MP.parse P.parseFile (unpack name) content of
        Left err -> pure $ T.err $ (("parse error: " <>) . pack . MP.errorBundlePretty) err
        Right rs -> T.ok <$> modifyContent pkgCtx content rs

errorPretty :: ProcessErr -> Text
errorPretty (msg, loc) =
    (TxT.pack . show . T.srcLine $ T.posOf loc)
    <> ": "
    <> snd (T.unLocated loc)
    <> "\nerror: "
    <> msg

sortImportsOn :: (Num n, Ord n) => (T.PackageInfo -> n) -> ProcessedAST -> ProcessedAST
sortImportsOn f ast = Util.reconstructFromIndexes (sortedPkgs <> nonPkgs)
    where
        extractPackages = Util.indexPairs ast
        sortedPkgs = sortOn (\(PImportGroup n _, _) -> f n) $ filter doFilter extractPackages
        nonPkgs = filter (not . doFilter) extractPackages
        doFilter (PImportGroup _ _, _) = True
        doFilter _ = False

stripPackageInfo :: ProcessedAST -> ProcessedAST
stripPackageInfo = sortOn sortFn . concatMap doStrip
    where
        sortFn (POldImportCmt line) = T.posOf line
        sortFn (PRawLine line) = T.posOf line
        doStrip :: ProcessedNode -> [ProcessedNode]
        doStrip (PImportGroup info imports) = map (PRawLine . fmap snd) imports
        doStrip x = [x]

stripPackageComments :: ProcessedAST -> ProcessedAST
stripPackageComments = stripPackageInfo . filter notOldCmt
    where
        notOldCmt (POldImportCmt _) = False
        notOldCmt _ = True

linesPreserve ""  = []
linesPreserve txt = TxT.split (=='\n') txt

unlinesPreserve []     = ""
unlinesPreserve [x]    = x
unlinesPreserve (x:"":xs) = x <> "\n" <> unlinesPreserve xs
unlinesPreserve (x:xs) = x <> "\n" <> unlinesPreserve xs

cmtToTxt (T.SingleLineCmt "") = ""
cmtToTxt (T.SingleLineCmt c) = "-- " <> c

unAST :: ProcessedAST -> Text
unAST = unlinesPreserve . map unpackAST . stripOld
    where
        unpackAST (PRawLine l) = T.unLocated l
        unpackAST (PImportGroup pkg imports) = unlinesPreserve $ cmtToTxt (packageToComment pkg):map (snd . T.unLocated) imports
        stripOld = filter (\case
                            POldImportCmt _ -> False
                            _ -> True
                            )


runWithCtxT :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> (ProcessedAST -> ProcessedAST) -> IO (Text, Text)
runWithCtxT mPkgCtx src t =
    either (mempty,) ok . join <$> sequence (flip parseToAST src <$> mPkgCtx)
    where
        ok :: (ProcessedAST, [ProcessErr]) -> (Text, Text)
        ok = bimap (unAST . t) (foldl (<>) mempty . map errorPretty)

runWithCtx :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> IO (Text, Text)
runWithCtx mPkgCtx src = runWithCtxT mPkgCtx src id


eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx

mkAndPopulateStackDb :: IO (T.Result GPKG.MapStore)
mkAndPopulateStackDb = GPKG.mkDbAndPopulate proc
    where
        proc = GPKG.pkgCmd (uncurry SP.proc)

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
        toCommentGroup (pkg, imports) = PImportGroup pkg imports
        oldComments pkgs = filter (`Set.notMember` pkgCmts pkgs) . Set.toList . pkgCmts . M.keysSet
            where
                pkgCmts = Set.map (cmtToTxt . packageToComment)
        commented :: Map T.PackageInfo [T.Located T.ImportDecl] -> ProcessedAST
        commented = map toCommentGroup . M.toList
        foldLines :: Map T.PackageInfo [T.Located T.ImportDecl] -> [ProcessedNode] -> Int -> Text -> [ProcessedNode]
        foldLines imports result lineNb line
            | maybe False ((lineNb ==) . fst) importsRange =
                result <> commented imports

            | commentOnLine lineNb && notPassthroughComment line = result <> [POldImportCmt (T.Located pos line)]
            | importOnLine lineNb = result
            | otherwise = nextV
            where
                pos = T.Pos lineNb
                nextV = result <> [PRawLine $ T.Located pos line]
                allPackages = Set.map packageToComment $ M.keysSet imports
                notPassthroughComment line = Set.member line (Set.map cmtToTxt allPackages)













