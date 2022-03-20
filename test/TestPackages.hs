{-# LANGUAGE OverloadedStrings #-}
module TestPackages (spec) where

import Test.Hspec
import qualified Packages as PKG
import Data.Text (Text)
import qualified Types as T

newtype MockTestProvider = MockTestProvider (Text -> T.PackageInfo)

instance Show MockTestProvider where
    show = const "MockTestProvider"

instance Eq MockTestProvider where
    l == r = True

instance PKG.MappingSource MockTestProvider where
    providerOfModule (MockTestProvider f) (T.ModuleName n) = pure $ Right (f n)

expectedPackaged = T.PackageInfo "testpkg"
testingCtx =
            (PKG.mkCtx
                (MockTestProvider (const expectedPackaged)))

spec :: Spec
spec = context "packages context" $ do
    describe "mocked backend" $ do
        it "lookups up text" $ do
            PKG.providerOf testingCtx (T.ModuleName "somemodule")
            >>= \v -> do
                fst v `shouldBe` (Right $ expectedPackaged)
                snd v `shouldBe` testingCtx
    describe "ghc-pkg backend" $ do
        it "matches base correct" $ do
            v <- PKG.providerOf PKG.mkGhcPkgCtx (T.ModuleName "Data.Maybe")
            fst v `shouldBe` Right (T.PackageInfo "base")


