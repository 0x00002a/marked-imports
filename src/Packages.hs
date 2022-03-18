{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Packages (mkCtx, MappingCtx, providerOf, MappingSource(..)) where

import qualified System.Process as SP
import qualified Types as T
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as TxT
import qualified Parser as P
import System.Exit (ExitCode(..))

mkCtx :: MappingSource s => s -> MappingCtx s
mkCtx = MappingCtx mempty

data MappingCtx a = MappingCtx { mCtxCache :: !(Map T.ModuleName T.PackageInfo), mCtxLookup :: !a } deriving(Eq, Show, Ord)

data GHCPkgSource = GHCPkgSource { ghcPkgCmd :: !Text }

class MappingSource a where
    providerOfModule :: a -> T.ModuleName -> IO (T.Result T.PackageInfo)

providerOf :: MappingSource s => MappingCtx s -> T.ModuleName -> IO (T.Result T.PackageInfo, MappingCtx s)
providerOf ctx name = case M.lookup name (mCtxCache ctx) of
    Nothing -> (,ctx) <$> providerOfModule (mCtxLookup ctx) name
    Just info -> pure (Right info, ctx)


instance MappingSource GHCPkgSource where
    providerOfModule = packageInfoFromGHC

packageInfoFromGHC :: GHCPkgSource -> T.ModuleName -> IO (T.Result T.PackageInfo)
packageInfoFromGHC ctx name = checkResult <$> runGhcPkg
    where
        checkResult (ExitSuccess, out, _) = parsePackageInfo $ TxT.pack out
        checkResult (_, _, err) = Left $ "error while running ghc-pkg: " <> TxT.pack err
        runGhcPkg :: IO (ExitCode, String, String)
        runGhcPkg =
            SP.readCreateProcessWithExitCode
                (SP.proc (TxT.unpack (ghcPkgCmd ctx)) ["--simple-output", "find-module", TxT.unpack (T.modName name)])
                ""

parsePackageInfo :: Text -> T.Result T.PackageInfo
parsePackageInfo txt = case MP.parseMaybe P.packageExpr txt of
    Nothing -> Left $ "failed to parse package: " <> txt
    Just v -> Right v






