{-# LANGUAGE OverloadedStrings #-}
module TestLUtil where

import Test.Hspec
import qualified LUtil as LU
import qualified Types as T

spec = describe "test util" $ do
    describe "mconcatInfix" $ do
        it "results in mempty for empty input" $ do
            LU.mconcatInfix "v" [] `shouldBe` mempty
        it "results in in input for single value" $ do
            LU.mconcatInfix "v" ["v1"] `shouldBe` "v1"
        it "results in 1 <> v <> 2 for multvalued input" $ do
            LU.mconcatInfix "|" ["v1", "v2"] `shouldBe` "v1|v2"
    describe "nameOfLocalPackage" $ do
        it "should be marked-imports" $ do
            n <- LU.nameOfLocalPackage
            n `shouldBe` (Just "marked-imports")
        it "fails if cannot find cabal before root" $ do
            n <- LU.nameOfLocalPackage' (pure . const [])
            n `shouldBe` Nothing
    describe "linesCoveredByImport" $ do
        it "should produce [line] for single line" $ do
            LU.linesCoveredByImport (T.Located (T.Pos 1) (T.ModuleName "X", "import Y")) `shouldMatchList` [1]
        it "should produce [line, line + 1] for multi line" $ do
            LU.linesCoveredByImport (T.Located (T.Pos 1) (T.ModuleName "X", "import Y\n(x\n)")) `shouldMatchList` [1, 2, 3]



