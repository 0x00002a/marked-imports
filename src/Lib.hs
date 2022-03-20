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
import Control.Arrow (first)
import Data.Bifunctor (Bifunctor(bimap))
import qualified LUtil as Util

run :: T.SourceInfo Text -> IO (Text, Text)
run (T.SourceInfo name content) = case MP.parse P.parseFile (unpack name) content of
    Left err -> pure (content, (("parse error: " <>) . pack . MP.errorBundlePretty) err)
    Right rs -> bimap TxT.unlines unpackErrs <$> modifyContent content rs
    where
      unpackErrs :: [T.Located T.ModuleName] -> Text
      unpackErrs [] = ""
      unpackErrs xs = "could not find packages for: \n-  " <> Util.mconcatInfix "\n-  " (map (T.modName . T.unLocated) xs)

eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx


pkgLookupCtx = PKG.mkDefaultCtx

packageToComment :: T.PackageInfo -> Text
packageToComment pkg = "-- " <> T.pkgName pkg

-- Second result is errors
extractImports :: T.Module -> IO (Map T.PackageInfo [T.Located T.ModuleName], [T.Located T.ModuleName])
extractImports mod = fst <$> foldlM doFold ((mempty, mempty), pkgLookupCtx) (T.modImports mod)
    where
        doFold ((xs, errs), ctx) name = do
            minfo <- addComment (T.unLocated name) ctx
            pure $ case minfo of
                Nothing -> ((xs, name:errs), ctx)
                Just (info, nextCtx) -> ((M.insert info (name:M.findWithDefault [] info xs) xs, errs), nextCtx)

addComment :: PKG.MappingSource s => T.ModuleName -> PKG.MappingCtx s -> IO (Maybe (T.PackageInfo, PKG.MappingCtx s))
addComment name ctx = applyCmt <$> wantedPkg name
    where
        wantedPkg n = PKG.providerOf ctx n
        applyCmt (Left _, _) = Nothing -- TODO: Report this error?
        applyCmt (Right rs, ctx) = Just (rs, ctx)

modifyContent :: Text -> T.Module -> IO ([Text], [T.Located T.ModuleName])
modifyContent txt mod = extractResult <$>
    foldlM (\xs x -> fmap inc (foldLines xs x))
            (1, ([], []), pkgLookupCtx)
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
                extracted <- extractImports mod
                pure (lineNb, xs <> first commented extracted, pkgCtx)
            | isJust (importOnLine lineNb) = pure inp
            | otherwise = pure nextV
            where
                nextV = (lineNb, first (++ [line]) xs, pkgCtx)













