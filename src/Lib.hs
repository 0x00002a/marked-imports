{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( run, runWithCtx, mkPkgLookupCtx, mkAndPopulateStackDb
    ) where

import qualified System.Process as SP
import Data.Text (Text, pack, unpack)
import qualified Data.Text as TxT
import Data.Foldable (toList)
import System.Exit (ExitCode(..))
import Data.Maybe (catMaybes, fromMaybe, fromJust, isJust)
import qualified Text.Megaparsec as MP
import qualified Parser as P
import qualified Types as T
import qualified Packages as PKG
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (foldM)
import Data.Foldable (foldlM)
import Control.Arrow (first, second)
import Data.Bifunctor (Bifunctor(bimap))
import qualified LUtil as Util
import qualified GhcPkg as GPKG
import qualified Data.Set as Set

newtype GhcPkgDbSource db = GhcPkgDbSource db

instance (GPKG.Database db) => PKG.MappingSource (GhcPkgDbSource db) where
    providerOfModule (GhcPkgDbSource db) name = pure $
            maybe (Left "could not find package") Right $ T.pkgInfo <$> GPKG.lookup db name

run :: T.SourceInfo Text -> IO (Text, Text)
run src = do
    ctx <- mkPkgLookupCtx
    runWithCtx ctx src

runWithCtx :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> IO (Text, Text)
runWithCtx mPkgCtx (T.SourceInfo name content) = do
    case mPkgCtx of
        Left err -> pure (mempty, err)
        Right pkgCtx ->
            case MP.parse P.parseFile (unpack name) content of
            Left err -> pure (content, (("parse error: " <>) . pack . MP.errorBundlePretty) err)
            Right rs -> bimap TxT.unlines unpackErrs <$> modifyContent pkgCtx content rs
    where
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

packageToComment :: T.PackageInfo -> Text
packageToComment pkg = packageCommentPrefix <> T.pkgName pkg

-- Second result is errors
extractImports ::
    PKG.MappingSource s => s
    -> T.Module
    -> IO (Map T.PackageInfo [T.Located T.ModuleName], [(Text, T.Located T.ModuleName)])
extractImports ctx mod = foldlM doFold (mempty, mempty) (T.modImports mod)
    where
        doFold (xs, errs) name = do
            minfo <- PKG.providerOfModule ctx (T.unLocated name)
            pure $ case minfo of
                Left err -> (xs, (err, name):errs)
                Right info -> (M.insert info (name:M.findWithDefault [] info xs) xs, errs)

modifyContent :: PKG.MappingSource s => s -> Text -> T.Module -> IO ([Text], [(Text, T.Located T.ModuleName)])
modifyContent mapCtx txt mod = do
    imports <- extractImports mapCtx mod
    let linesOut = foldl (\xs x -> first (+1) (foldLines (fst imports) xs x))
            (1, mempty)
            lines
    pure (snd linesOut, snd imports)
    where
        lines = TxT.lines txt
        txtForImport imp = lines !! ((T.srcLine $ T.posOf imp) - 1)
        importOnLine line = isJust $ find (==line) linesForComment
        commentOnLine line = isJust $ find (==line) $ map (T.srcLine . T.posOf) (T.modComments mod)
        linesForComment = filter (not . commentOnLine . (\x -> x - 1)) importLines
        importLines = map (T.srcLine . T.posOf) $ T.modImports mod
        importsRange = case importLines of
            [] -> Nothing
            lines -> Just (minimum lines, maximum lines)
        toCommentGroup (pkg, imports) = packageToComment pkg:map txtForImport imports
        commented :: Map T.PackageInfo [T.Located T.ModuleName] -> [Text]
        commented = concatMap toCommentGroup . M.toList
        foldLines :: Map T.PackageInfo [T.Located T.ModuleName] -> (Int, [Text]) -> Text -> (Int, [Text])
        foldLines imports inp@(lineNb, result) line
            | maybe False ((lineNb ==) . fst) importsRange = do
                (lineNb, result <> commented imports)
            | importOnLine lineNb || (commentOnLine lineNb && notPassthroughComment line) = inp
            | otherwise = nextV
            where
                nextV = (lineNb, result <> [line])
                allPackages = Set.map packageToComment $ M.keysSet imports
                notPassthroughComment line = Set.member line allPackages













