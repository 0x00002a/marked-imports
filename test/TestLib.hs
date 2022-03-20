{-# LANGUAGE OverloadedStrings #-}
module TestLib where
import qualified Lib as L
import qualified Types as T

import Test.Hspec

exampleCommentedInput = "module T where\n-- text\nimport Data.Text\n"
testSrc = T.SourceInfo ""
shouldOutputOk rs out = rs `shouldBe` (out, mempty)

spec = context "lib tests" $ do
    it "doesn't add additional comments for already commented input" $ do
        rs <- L.run (testSrc exampleCommentedInput)
        rs `shouldOutputOk` exampleCommentedInput

