{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Packages (mkLocalMatcher, MappingSource(..), LocalPkgMatcher) where

import           Control.Monad.Reader ( Reader, runReader )
import           Data.List            ( find )
import           Data.Map             ( Map )
import qualified Data.Map             as M
import           Data.Maybe           ( fromMaybe )
import qualified Data.Set             as Set
import           Data.Text            ( Text )
import qualified Data.Text            as TxT
import qualified LUtil                as Util
import qualified Parser               as P
import qualified System.Directory     as SD
import           System.Exit          ( ExitCode (..) )
import qualified System.Process       as SP
import qualified Text.Megaparsec      as MP
import qualified Types                as T

mkLocalMatcher :: MappingSource s => s -> LocalPkgMatcher s
mkLocalMatcher = LocalPkgMatcher

newtype LocalPkgMatcher a = LocalPkgMatcher a

class MappingSource a where
    providerOfModule :: a -> T.ModuleName -> IO (T.Result T.PackageInfo)

cmdName :: SP.CmdSpec -> String
cmdName (SP.RawCommand n _) = n
cmdName (SP.ShellCommand n) = n

instance MappingSource s => MappingSource (LocalPkgMatcher s) where
    providerOfModule (LocalPkgMatcher ctx) name = do
        minfo <- wrapped
        mlocalName <- Util.nameOfLocalPackage
        pure $ do
            info <- minfo
            pkgName <- maybe (T.err "could not find cabal file") T.ok mlocalName
            pure $ if T.pkgName info == pkgName
                then T.PackageInfo "local"
                else info
        where
            wrapped = providerOfModule ctx name




