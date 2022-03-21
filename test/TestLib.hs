{-# LANGUAGE OverloadedStrings #-}
module TestLib where
import qualified Lib as L
import qualified Types as T
import Data.Text (Text)
import qualified Data.Text as TxT
import Control.Monad ((>=>))
import LUtil ((><>))
import qualified LUtil as Util
import Data.Char (isAlphaNum, isDigit, isUpper, isLower)
import qualified Util as TUtil

import Test.Hspec

import Test.QuickCheck as QC

newtype RunResults = RunResults (Text -> IO (Text, Text))
newtype RunInput = RunInput Text

instance QC.Arbitrary RunResults where
    arbitrary = foldl1 (\(RunResults last) (RunResults x) -> RunResults (last >=> \(out, _) -> x out)) <$> QC.listOf1 (pure $ RunResults (L.run . testSrc))

instance QC.Arbitrary RunInput where
    arbitrary = fmap RunInput $ (pure "module T where\nimport ") ><> ((Util.mconcatInfix "\nimport " . map T.modName) <$> QC.listOf (arbitrary :: Gen T.ModuleName))
instance QC.Arbitrary T.ModuleName where
    arbitrary = T.ModuleName . TxT.pack <$> (QC.suchThat (arbitrary :: Gen String) (\s -> s != [] && all (\x -> isUpper x || isDigit x) s))

exampleCommentedInput = "module T where\n-- text\nimport Data.Text\n"
testSrc = T.SourceInfo ""
shouldOutputOk rs out = rs `shouldBe` (out, mempty)

spec = context "lib tests" $ do
    it "doesn't add additional comments for already commented input" $ do
        rs <- L.runWithCtx (TUtil.mkDummyCtx "text") (testSrc exampleCommentedInput)
        rs `shouldOutputOk` exampleCommentedInput
    it "produces same output from run to run" $ do
        modules <- QC.generate $ QC.listOf1 (pure "-- text\nData.Text\n")
        let doc = "module T where\nimport " <> Util.mconcatInfix "\nimport " modules
        rs <- L.runWithCtx (TUtil.mkDummyCtx "text") (testSrc doc)
        rs `shouldOutputOk` doc

