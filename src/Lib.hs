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


run :: T.SourceInfo Text -> IO Text
run (T.SourceInfo name content) = case MP.parse P.parseFile (unpack name) content of
    Left err -> (pure . ("parse error: " <>) . pack . MP.errorBundlePretty) err
    Right rs -> TxT.unlines<$> modifyContent content rs

eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx


pkgLookupCtx = PKG.mkStackCtx PKG.mkGhcPkgCtx

packageToComment :: T.PackageInfo -> Text
packageToComment pkg = "-- " <> T.pkgName pkg

extractImports :: T.Module -> IO (Map T.PackageInfo [T.Located T.ModuleName])
extractImports mod = fst <$> foldlM doFold (mempty, pkgLookupCtx) (T.modImports mod)
    where
        doFold (xs, ctx) name = do
            (info, nextCtx) <- fromJust <$> addComment (T.unLocated name) ctx
            pure (M.insert info (name:M.findWithDefault [] info xs) xs, nextCtx)

addComment :: PKG.MappingSource s => T.ModuleName -> PKG.MappingCtx s -> IO (Maybe (T.PackageInfo, PKG.MappingCtx s))
addComment name ctx = applyCmt <$> wantedPkg name
    where
        wantedPkg n = PKG.providerOf ctx n
        applyCmt (Left _, _) = Nothing -- TODO: Report this error?
        applyCmt (Right rs, ctx) = Just (rs, ctx)

modifyContent :: Text -> T.Module -> IO [Text]
modifyContent txt mod = extractResult <$>
    foldl (\xs x -> xs >>= (\l -> inc <$> (foldLines l x)))
            (pure (1, [], pkgLookupCtx))
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
        foldLines inp@(lineNb, result, pkgCtx) line
            | lineNb == fst importsRange = do
                extracted <- extractImports mod
                let commented = concatMap (\(pkg, imports) -> packageToComment pkg:map txtForImport imports) $ M.toList extracted
                pure (lineNb, result ++ commented, pkgCtx)
            | isJust (importOnLine lineNb) = pure inp
            | otherwise = pure nextV
            where
                nextV = (lineNb, result ++ [line], pkgCtx)













