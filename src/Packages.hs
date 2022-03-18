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


newtype MappingCtx = MappingCtx { mCtxCache :: Map T.ModuleName T.PackageInfo }

providerOf :: MappingCtx -> T.ModuleName -> IO (T.Result T.PackageInfo, MappingCtx)
providerOf ctx name = case M.lookup name (mCtxCache ctx) of
    Nothing -> undefined
    Just info -> pure (Right info, ctx)

packageInfoFromGHC :: T.ModuleName -> IO (T.Result T.PackageInfo)
packageInfoFromGHC name = undefined

parsePackageInfo :: Text -> T.Result T.PackageInfo
parsePackageInfo txt = case MP.parseMaybe P.packageExpr txt of
    Nothing -> Left $ "failed to parse package: " <> txt
    Just v -> Right v






