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


exampleCommentedInput = "module T where\n-- text\nimport Data.Text\n"
testSrc = T.SourceInfo ""
shouldOutputOk rs out = rs `shouldBe` (out, mempty)

spec = context "lib tests" $ do
    it "doesn't add additional comments for already commented input" $ do
        rs <- L.runWithCtx (TUtil.mkDummyCtx "text") (testSrc exampleCommentedInput)
        rs `shouldOutputOk` exampleCommentedInput
    it "produces same output from run to run" $ do
        let modules = replicate 5 "Data.Text" <> replicate 3 "Data.Maybe"
        let doc = "module T where\n-- text\nimport " <> Util.mconcatInfix "\nimport " modules <> "\n"
        let runner doc = L.runWithCtx (TUtil.mkDummyCtx "text") (testSrc doc)
        rs <- runner doc
        rs' <- runner (fst rs)
        rs'' <- runner (fst rs')
        rs'' `shouldOutputOk` doc

