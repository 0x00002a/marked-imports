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
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import qualified Text.Megaparsec as MP
import qualified Parser as P
import qualified Types as T
import qualified Packages as PKG
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (foldM)
import Data.Foldable (foldlM)

type GhcPkgMapping = PKG.MappingCtx PKG.GHCPkgSource

run :: T.SourceInfo Text -> IO Text
run (T.SourceInfo name content) = case MP.parse P.parseFile (unpack name) content of
    Left err -> (pure . ("parse error: " <>) . pack . MP.errorBundlePretty) err
    Right rs -> TxT.unlines<$> modifyContent content rs

eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx


packageToComment :: T.PackageInfo -> Text
packageToComment (T.PackageInfo name _) = "-- " <> name

extractImports :: T.Module -> IO (Map T.PackageInfo [T.ModuleName])
extractImports mod = fst <$> foldlM doFold (mempty, PKG.mkGhcPkgCtx) (map T.unLocated $ T.modImports mod)
    where
        doFold (xs, ctx) name = do
            (info, nextCtx) <- fromJust <$> addComment name ctx
            pure (M.insert info (M.findWithDefault [] info xs) xs, nextCtx)

addComment :: T.ModuleName -> GhcPkgMapping -> IO (Maybe (T.PackageInfo, GhcPkgMapping))
addComment name ctx = applyCmt <$> wantedPkg name
    where
        wantedPkg n = PKG.providerOf ctx n
        applyCmt (Left _, _) = Nothing -- TODO: Report this error?
        applyCmt (Right rs, ctx) = Just (rs, ctx)

modifyContent :: Text -> T.Module -> IO [Text]
modifyContent txt mod = extractResult <$>
    foldl (\xs x -> xs >>= (\l -> inc <$> (foldLines l x)))
            (pure (1, [], PKG.mkGhcPkgCtx))
            (TxT.lines txt)
    where
        inc (line, x, y) = (line + 1, x, y)
        extractResult (_, r, _) = reverse r
        --sortedMod = mod { T.modImports = sort (T.modImports mod), T.modComments = sort (T.modComments mod) }
        --importOnLine :: Maybe (T.Located T.ModuleName)
        importOnLine line = find (eqByLine (T.Pos line)) (T.modImports mod)
        commentOnLine line = find (eqByLine (T.Pos line)) (T.modComments mod)

        --foldLines :: (Int, [Text]) -> Text -> IO (Int, [Text])
        foldLines inp@(lineNb, result, pkgCtx) line =
            case importOnLine lineNb of
                Nothing -> pure nextV
                Just mod ->
                    case commentOnLine (lineNb - 1) of
                        Just _ -> pure nextV -- ignore imports with a comment above already
                        Nothing -> do
                            cmt <- addComment (T.unLocated mod) pkgCtx
                            case cmt of
                                Nothing -> pure nextV
                                Just (info, ctx) -> pure (lineNb, line:packageToComment info:result, ctx)
            where
                nextV = (lineNb, line:result, pkgCtx)













