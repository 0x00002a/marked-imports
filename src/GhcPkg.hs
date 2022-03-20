{-# LANGUAGE OverloadedStrings #-}
module GhcPkg (
    Database(..)
  , mkDbAndPopulate
  , pkgCmd
  , unPkgCmd
  , populateDb
  , MapStore

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

newtype GhcPkgCmd = GhcPkgCmd ([String] -> SP.CreateProcess)

class Database a where
    insert :: a -> T.PackageSpec -> a
    lookup :: a -> T.ModuleName -> Maybe T.PackageSpec


newtype MapStore = MapStore (M.Map T.ModuleName T.PackageSpec) deriving (Eq)
unMapStore (MapStore m) = m

instance Semigroup MapStore where
    l <> r = MapStore $ unMapStore l <> unMapStore r
instance Monoid MapStore where
    mempty = MapStore mempty

instance Database MapStore where
    insert (MapStore m) spec = MapStore $ foldl (\m n -> M.insert n spec m) m $ T.pkgExposes spec
    lookup (MapStore m) info =  M.lookup info m

unPkgCmd (GhcPkgCmd v) = v
pkgCmd :: ((String, [String]) -> SP.CreateProcess) -> GhcPkgCmd
pkgCmd f = GhcPkgCmd (\args -> f ("ghc-pkg", args))

mkDbAndPopulate :: (Monoid s, Database s) => GhcPkgCmd -> IO (T.Result s)
mkDbAndPopulate = populateDb mempty

populateDb :: Database s => s -> GhcPkgCmd -> IO (T.Result s)
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
        foldResults rs = foldl insert db rs



