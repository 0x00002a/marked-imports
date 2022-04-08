{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts     #-}

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
import Debug.Trace (traceShowId)
import Data.List (sort)


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

checkWithImports :: [Text] -> (Text -> Text) -> IO ()
checkWithImports imports pkgLookup = do
    rs <- runner (doc (const Nothing))
    rs `shouldOutputOk` doc (Just . pkgLookup)
    where
        runner doc = L.runWithCtx (TUtil.mkFuncCtx pkgLookup) (testSrc doc)
        doc f =
            "module T where\n"
            <> foldl (\xs m -> xs <> "\n" <> maybe "" ("-- " <>) (f m) <> "\nimport " <> m) mempty imports
            <> "\n"

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
    it "handles multiline explicit imports" $ do
        let modules = ["S", "Data.Maybe \n(Maybe\n)"]
        let funcCtx "S" = "m1"
            funcCtx _ = "z"
        checkWithImports modules funcCtx
    it "strips whitespace right" $ do
        let input = [r|
module M where

{- 
    Some

    Text
    -}
-- test
import X.Y
import H.Z
        |]
        rs <- L.runWithCtxT (TUtil.mkDummyCtx "test") (testSrc input) L.stripWhitespaceBetweenImports
        rs `shouldOutputOk` input
    it "strips whitespace after last import" $ do
        let input = [r|module M where


-- test
import X.Y
import H.Z


some text
        |]

        let stripped = [r|module M where
-- test
import X.Y
import H.Z
some text
        |]
        rs <- L.runWithCtxT (TUtil.mkDummyCtx "test") (testSrc input) (L.stripWhitespaceBetweenImports)
        rs `shouldOutputOk` stripped
    describe "addLinesBeforeGroups" $ do
        it "adds n blank lines before and after" $ do
            let input = [
                    L.PImportGroup (T.Located 1 (T.PackageInfo ""))
                        [T.Located 1 (T.ModuleName "M", "import M")],
                    L.PImportGroup (T.Located 2 (T.PackageInfo ""))
                        [T.Located 2 (T.ModuleName "Y", "import Y")]
                        ]
            let input' = [
                            L.PRawLine (T.Located 1 ""),
                            L.PImportGroup (T.Located 2 (T.PackageInfo ""))
                                [T.Located 1 (T.ModuleName "M", "import M")],
                            L.PRawLine (T.Located 3 ""),
                            L.PImportGroup (T.Located 4 (T.PackageInfo ""))
                                [T.Located 2 (T.ModuleName "Y", "import Y")],
                            L.PRawLine (T.Located 5 "")
                        ]
            let rs = L.addLinesBeforeGroups 1 input
            rs `shouldMatchList` input'
    it "handles case with =>" $ do
        let input = [r|
module M where

-- local
import X

-- test
class (y x, z (m h)) => z
        |]
        let runner = L.runWithCtx (TUtil.mkDummyCtx "local") (testSrc input)
        rs <- runner
        rs `shouldOutputOk` input

