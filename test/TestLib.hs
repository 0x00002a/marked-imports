{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module TestLib where
import           Control.Monad     (when, (>=>))
import           Data.Char         (isAlphaNum, isDigit, isLower, isUpper)
import           Data.Text         (Text)
import qualified Data.Text         as TxT
import           LUtil             ((><>))
import qualified LUtil             as Util
import qualified Lib               as L
import qualified Types             as T
import qualified Util              as TUtil

import           Test.Hspec

import           Data.Either       (fromRight)
import qualified Packages          as PKG
import           Test.QuickCheck   as QC
import           Text.RawString.QQ


exampleCommentedInput = "module T where\n-- text\nimport Data.Text\n"
testSrc = T.SourceInfo ""
shouldOutputOk rs out = rs `shouldBe` (out, mempty)

data WithFallbackSource s f = WithFallbackSource s f
instance (PKG.MappingSource f, PKG.MappingSource s) => PKG.MappingSource (WithFallbackSource s f) where
    providerOfModule (WithFallbackSource main fallback) pkg = do
        rsmain <- PKG.providerOfModule main pkg
        case rsmain of
            Right ok -> pure $ Right ok
            Left err -> do
                rsfall <- PKG.providerOfModule fallback pkg
                pure $ case rsfall of
                    Right ok -> Right ok
                    Left errfall -> Left ("first: " <> err <> ";second: " <> errfall)

testCtxFb fall = do
    base <- L.mkPkgLookupCtx
    pure $ case base of
        Left err -> Left err
        Right ok -> Right $ WithFallbackSource ok fall

test :: PKG.MappingSource s => T.Result s -> IO ()
test = const $ pure ()

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
    it "updates out of date docs" $ do
        let modules = ["Data.Maybe"]
        let doc name = "module T where\n-- " <> name <> "\nimport " <> Util.mconcatInfix "\nimport " modules <> "\n"
        let runner doc = L.runWithCtx (TUtil.mkDummyCtx "text") (testSrc doc)
        rs <- runner $ doc "text1"
        rs `shouldOutputOk` doc "text1\n-- text"
