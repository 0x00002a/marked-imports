{-# LANGUAGE OverloadedStrings #-}
module GhcPkg (
    mkDefaultDb
  , BackingStore(..)
  , Database
  , mkDb
  , pkgCmd
  , unPkgCmd
  , populateDb

) where

import Data.Text (Text)
import qualified Data.Text as TxT
import qualified Types as T
import qualified System.Process as SP
import System.Exit (ExitCode(..))
import qualified Parser as P
import qualified Text.Megaparsec as MP
import qualified LUtil as Util
import qualified Data.Map as M

import Prelude hiding (lookup)

mkDb :: BackingStore s => s -> Database s
mkDb = Database

mkDefaultDb :: Database MapStore
mkDefaultDb = Database { dbStore = MapStore mempty }

newtype GhcPkgCmd = GhcPkgCmd ([Text] -> SP.CreateProcess)

class BackingStore a where
    insert :: a -> T.PackageSpec -> a
    lookup :: a -> T.PackageInfo -> Maybe T.PackageSpec


data Database s = Database { dbStore :: !s }

instance BackingStore s => BackingStore (Database s) where
    insert db spec = db { dbStore = insert (dbStore db) spec }
    lookup db info = lookup (dbStore db) info

newtype MapStore = MapStore (M.Map T.PackageInfo T.PackageSpec)

instance BackingStore MapStore where
    insert (MapStore m) spec = MapStore $ M.insert (T.pkgInfo spec) spec m
    lookup (MapStore m) info = M.lookup info m

unPkgCmd (GhcPkgCmd v) = v
pkgCmd = GhcPkgCmd

populateDb :: BackingStore s => Database s -> GhcPkgCmd -> IO (T.Result (Database s))
populateDb db cmd = do
    let p = unPkgCmd cmd ["dump"]
    rs <- SP.readCreateProcessWithExitCode p ""
    pure $ case rs of
        (ExitSuccess, out, _) ->
            Util.mapLeft
            (TxT.pack . MP.errorBundlePretty) $
            foldResults <$> MP.parse P.ghcPkgDump "ghc-pkg dump" (TxT.pack out)
        (ExitFailure code, out, err) -> T.err $ "failed to dump ghc pkg db: " <> TxT.pack (show code) <> "\n" <> TxT.pack out <>TxT.pack err
    where
        foldResults rs = foldl (\db x -> db { dbStore = insert (dbStore db) x }) db rs



