{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Packages (mkGhcPkgCtx, mkCtx, GHCPkgSource, MappingCtx, providerOf, MappingSource(..)) where

import qualified System.Process as SP
import qualified Types as T
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Text.Megaparsec as MP
import qualified Data.Text as TxT
import qualified Parser as P
import System.Exit (ExitCode(..))

mkGhcPkgCtx :: MappingCtx GHCPkgSource
mkGhcPkgCtx = mkCtx $ GHCPkgSource "ghc-pkg"

mkStackCtx :: RunnableProcess a => MappingCtx a -> MappingCtx (StackEnv a)
mkStackCtx = fmap StackEnv

mkCtx :: MappingSource s => s -> MappingCtx s
mkCtx = MappingCtx mempty

data MappingCtx a = MappingCtx { mCtxCache :: !(Map T.ModuleName T.PackageInfo), mCtxLookup :: !a } deriving(Eq, Show, Ord)

data GHCPkgSource = GHCPkgSource { ghcPkgCmd :: !Text }

newtype StackEnv a = StackEnv a

class RunnableProcess a where
    process :: a -> T.ModuleName -> SP.CreateProcess
    pname :: a -> Text

class MappingSource a where
    providerOfModule :: a -> T.ModuleName -> IO (T.Result T.PackageInfo)

instance MappingSource GHCPkgSource where
    providerOfModule = packageInfoFromGHC

instance Functor MappingCtx where
    fmap f ctx = ctx { mCtxLookup = f (mCtxLookup ctx) }

cmdName :: SP.CmdSpec -> String
cmdName (SP.RawCommand n _) = n
cmdName (SP.ShellCommand n) = n

instance RunnableProcess a => RunnableProcess (StackEnv a) where
    process (StackEnv comp) n = extractComp (SP.cmdspec $ process comp n)
        where
            extractComp (SP.RawCommand cmd args) = SP.proc "stack" $ ["--", cmd] ++ args
            extractComp (SP.ShellCommand cmd) = SP.shell $ "stack -- " <> cmd
    pname (StackEnv comp) = "stack/" <> pname comp

instance RunnableProcess GHCPkgSource where
    process ctx name = SP.proc (TxT.unpack (ghcPkgCmd ctx)) ["--simple-output", "--names-only", "find-module", TxT.unpack (T.modName name)]
    pname = ghcPkgCmd

providerOf :: MappingSource s => MappingCtx s -> T.ModuleName -> IO (T.Result T.PackageInfo, MappingCtx s)
providerOf ctx name = case M.lookup name (mCtxCache ctx) of
    Nothing -> (,ctx) <$> providerOfModule (mCtxLookup ctx) name
    Just info -> pure (Right info, ctx)



packageInfoFromGHC :: RunnableProcess a => a -> T.ModuleName -> IO (T.Result T.PackageInfo)
packageInfoFromGHC ctx name = checkResult <$> runGhcPkg
    where
        checkResult (ExitSuccess, out, _) = parsePackageInfo $ TxT.pack out
        checkResult (_, _, err) = Left $ "error while running " <> pname ctx <> ": " <> TxT.pack err
        runGhcPkg :: IO (ExitCode, String, String)
        runGhcPkg =
            SP.readCreateProcessWithExitCode
                (process ctx name)
                ""

parsePackageInfo :: Text -> T.Result T.PackageInfo
parsePackageInfo txt = case MP.parseMaybe (P.packageExpr <* P.consumeLine_) txt of
    Nothing -> Left $ "failed to parse package: " <> txt
    Just v -> Right v






