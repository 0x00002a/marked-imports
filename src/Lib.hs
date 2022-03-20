{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( run
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
import qualified System.Process as SP

newtype GhcPkgDbSource db = GhcPkgDbSource db

instance (GPKG.Database db) => PKG.MappingSource (GhcPkgDbSource db) where
    providerOfModule (GhcPkgDbSource db) name = pure $
            maybe (Left "could not find package") Right $ T.pkgInfo <$> GPKG.lookup db name

run :: T.SourceInfo Text -> IO (Text, Text)
run (T.SourceInfo name content) = do
    mpkgCtx <- mkPkgLookupCtx
    case mpkgCtx of
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


mkPkgLookupCtx :: IO (T.Result (PKG.MappingCtx (PKG.LocalPkgMatcher (GhcPkgDbSource GPKG.MapStore))))
mkPkgLookupCtx = do
    db <- ctx
    pure $ PKG.mkCtx . PKG.mkLocalMatcher <$> db
    where
        ctx = do
            db <- GPKG.mkDbAndPopulate proc
            pure $ GhcPkgDbSource <$> db
        proc = GPKG.pkgCmd (\(cmd, args) -> SP.proc "stack" (["exec", "--", cmd] ++ args))

packageToComment :: T.PackageInfo -> Text
packageToComment pkg = "-- " <> T.pkgName pkg

-- Second result is errors
extractImports ::
    PKG.MappingSource s => PKG.MappingCtx s
    -> T.Module
    -> IO (Map T.PackageInfo [T.Located T.ModuleName], [(Text, T.Located T.ModuleName)])
extractImports pkgLookupCtx mod = fst <$> foldlM doFold ((mempty, mempty), pkgLookupCtx) (T.modImports mod)
    where
        doFold ((xs, errs), ctx) name = do
            (minfo, nextCtx) <- PKG.providerOf ctx (T.unLocated name)
            pure $ case minfo of
                Left err -> ((xs, (err, name):errs), nextCtx)
                Right info -> ((M.insert info (name:M.findWithDefault [] info xs) xs, errs), nextCtx)

modifyContent :: PKG.MappingSource s => PKG.MappingCtx s -> Text -> T.Module -> IO ([Text], [(Text, T.Located T.ModuleName)])
modifyContent mapCtx txt mod = extractResult <$>
    foldlM (\xs x -> fmap inc (foldLines xs x))
            (1, ([], []), mapCtx)
            lines
    where
        lines = TxT.lines txt
        txtForImport imp = lines !! ((T.srcLine $ T.posOf imp) - 1)
        inc (line, x, y) = (line + 1, x, y)
        extractResult (_, r, _) = r
        --sortedMod = mod { T.modImports = sort (T.modImports mod), T.modComments = sort (T.modComments mod) }
        --importOnLine :: Maybe (T.Located T.ModuleName)
        importOnLine line = find (eqByLine (T.Pos line)) (T.modImports mod)
        commentOnLine line = find (eqByLine (T.Pos line)) (T.modComments mod)
        importLines = map (T.srcLine . T.posOf) $ T.modImports mod
        importsRange = (minimum importLines, maximum importLines)

        --foldLines :: (Int, [Text], GhcPkgMapping) -> Text -> IO (Int, [Text], GhcPkgMapping)
        toCommentGroup (pkg, imports) = packageToComment pkg:map txtForImport imports
        commented = concatMap toCommentGroup . M.toList
        foldLines inp@(lineNb, xs@(result, errs), pkgCtx) line
            | lineNb == fst importsRange = do
                extracted <- extractImports pkgCtx mod
                pure (lineNb, xs <> first commented extracted, pkgCtx)
            | isJust (importOnLine lineNb) = pure inp
            | otherwise = pure nextV
            where
                nextV = (lineNb, first (++ [line]) xs, pkgCtx)













