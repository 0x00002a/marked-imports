{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Packages (MappingCtx, providerOf) where

import qualified System.Process as SP
import qualified Types as T
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as TxT
import qualified Parser as P


data MappingCtx = MappingCtx { mCtxCache :: !(Map T.ModuleName T.PackageInfo), ghcPkgCmd :: !Text }

providerOf :: MappingCtx -> T.ModuleName -> IO (T.Result T.PackageInfo, MappingCtx)
providerOf ctx name = case M.lookup name (mCtxCache ctx) of
    Nothing -> (,ctx) <$> packageInfoFromGHC ctx name
    Just info -> pure (Right info, ctx)

packageInfoFromGHC :: MappingCtx -> T.ModuleName -> IO (T.Result T.PackageInfo)
packageInfoFromGHC ctx name = checkResult <$> runGhcPkg
    where
        checkResult (ExitSuccess, out, _) = parsePackageInfo $ TxT.pack out
        checkResult (_, _, err) = Left $ "error while running ghc-pkg: " <> err
        runGhcPkg =
            SP.readCreateProcessWithExitCode
                (TxT.unpack $ ghcPkgCmd ctx)
                ["--simple-output", "find-module", TxT.unpack (T.modName name)]

parsePackageInfo :: Text -> T.Result T.PackageInfo
parsePackageInfo txt = case MP.parseMaybe P.packageExpr txt of
    Nothing -> Left $ "failed to parse package: " <> txt
    Just v -> Right v






