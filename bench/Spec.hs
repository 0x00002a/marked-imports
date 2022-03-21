{-# LANGUAGE OverloadedStrings #-}
import Criterion
import qualified Lib as L
import qualified GhcPkg as GPKG
import qualified Packages as PKG
import Data.Text (Text)
import qualified Types as T
import qualified LUtil as Util
import Criterion.Main (defaultMain)
import Control.Monad (void)


newtype ConstSource = ConstSource Text
instance PKG.MappingSource ConstSource where
    providerOfModule (ConstSource s) _ = pure $ pure (T.PackageInfo s)

mkDummyCtx :: IO (T.Result ConstSource)
mkDummyCtx = pure $ pure $ ConstSource "test"

specInput :: T.SourceInfo Text
specInput = T.SourceInfo "bench" $ "module MyModule where\n" <> (Util.mconcatInfix "\n" $ map ("import " <>) ["Data.Text", "Data.Maybe", "Control.Applicative", "Control.Monad", "Data.Either", "Text.Megaparsec"])

runN :: (Monad m, Num n, Eq n, Ord n) => m a -> n -> m a
runN act 1 = act
runN act n | n <= 0 = undefined
           | otherwise = act >> runN act (n - 1)

spec = [
    bench "constant context" $ toBenchmarkable (runN (void $ mkDummyCtx >>= (flip L.runWithCtx specInput)))
   ,bench "database" $ toBenchmarkable (runN (void $ L.mkPkgLookupCtx >>= (flip L.runWithCtx specInput)))
   ,bench "parsing of dump" $ toBenchmarkable (runN (void $ L.mkPkgLookupCtx))
    ]
main = defaultMain spec

