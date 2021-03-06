{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Monad     (join)
import           Data.Either       (fromRight, isLeft)
import           Data.Foldable     (asum)
import qualified Parser            as P
import           Test.Hspec
import qualified TestLUtil         as TestUtil
import qualified TestLib           as TestLib
import qualified TestPackages      as TestPkgs
import qualified Text.Megaparsec   as MP
import           Text.RawString.QQ
import qualified Types             as T
import           Util              (toPretty)

parse f = toPretty . MP.parse f ""


moduleTxt qualifiers name = ("import " <> qualifiers <> " " <> name, T.ModuleName name)

shouldBeOk parser check = parser `shouldBe` Right check

someplace = T.Located (T.Pos undefined)

importDeclSuite = context "import decl suite" $ do
            it "can parse an unqualified line" $ do
                parse P.importDecl (txt "") `shouldBeOk` expected
            it "can parse a qualified import" $ do
                parse P.importDecl (txt "qualified") `shouldBeOk` expectedQual "qualified"
            it "fails to parse an empty" $ do
                parse P.importDecl "" `shouldSatisfy` isLeft
            it "parses multiline with first non-explicit correctly" $ do
                let txt = "import Z" <> "\n" <> "import H \n(M\n)"
                let rs = parse (MP.some P.importDecl) txt
                rs `shouldBeOk` [(T.ModuleName "Z", "import Z"), (T.ModuleName "H", "import H \n(M\n)")]
            it "parses multiline with newline start" $ do
                let txt = "import Z\n   ( thing1\n, thing2 )"
                let rs = parse P.importDecl txt
                rs `shouldBeOk` (T.ModuleName "Z", txt)
    where
        base qual = moduleTxt qual "X.Y"
        expectedQual q = (snd $ base q, fst $ base q)
        expected = expectedQual ""
        txt = fst . base

commentDeclSuite = context "comment decl suite" $ do
    it "parses --text as a single line comment" $ do
        parse P.commentDecl "--text" `shouldBeOk` T.SingleLineCmt "text"
    it "parses {- text -} as a multiline comment spanning 1 lines" $ do
        parse P.commentDecl "{- text -}" `shouldBeOk` T.MultiLineCmt " text " 1

moduleHeaderSuite = context "module header suite" $ do
    it "parses module header without comment correctly" $ do
        parse P.moduleDecl "module X\nimport Y" `shouldBeOk` expectedImport 2 []
    it "parses module header correctly" $ do
        parse P.moduleDecl "module X\n--comment\nimport Y" `shouldBeOk` expectedImport 3 [(T.Located (T.Pos 2) (T.SingleLineCmt "comment"))]
    it "skips language pargmas" $ do
        parse P.parseFile "{-# LANGUAGE test me #-}\nmodule X\nimport Y" `shouldBeOk` expectedImport 3 []
    it "skips language pargmas" $ do
        parse P.moduleDecl "module X (x,\ny)\nimport Y" `shouldBeOk` expectedImport 3 []
    it "parses multiline import" $ do
        let base qual = moduleTxt qual "X.Y"
        let mutliLineOpts = ["\n(catMaybes\n,something\n)", "(catMaybes,something)", "(catMaybes\n,something)"]
        let txt = fst . base
        let results = map (\ls -> parse P.moduleDecl $ "module X.Y\n" <> txt "" <> ls <> "\n" <> ls) mutliLineOpts
        let expectedMutliline = snd $ base ""
        let check xs = all ((== expectedMutliline) . T.unLocated) xs
        mapM_ ((`shouldSatisfy` check) . map (fmap fst) . T.modImports . fromRight undefined) results
    it "parses multiline import with multilined import lists" $ do
        let txt = [r|module X.Y where

import           X.Z
    ( thing1
    , thing2
    )
import           Control.Lens
    ( Getting
    , Prism'
    , preview
    )
import           Control.Lens.Combinators                          (review)
        |]
        let expected = reverse $ map T.ModuleName ["X.Z", "Control.Lens", "Control.Lens.Combinators"]
        let result = map (fst . T.unLocated) . T.modImports <$> parse P.moduleDecl txt
        result `shouldBeOk` expected
    where
        expectedImport n = T.Module [(T.Located (T.Pos n) ((T.ModuleName "Y", "import Y")))]

packageExprSuite = context "package expression" $ do
    it "splits name properly" $
        parse (P.packageExpr) "example" `shouldBeOk` T.PackageInfo "example"
    it "splits name-version properly" $
        parse (P.packageExpr) "example-0.13.3" `shouldBeOk` T.PackageInfo "example"
    it "handles name-with-dash-version properly" $ do
        parse (P.packageExpr) "example-dash-0.1.2" `shouldBeOk` T.PackageInfo "example-dash"
    it "handles name-with-dash-second-version properly" $ do
        parse (P.packageExpr) "example-dash-thesec-2.1.2" `shouldBeOk` T.PackageInfo "example-dash-thesec"

packageSpecSuite = context "package spec" $ do
    it "fails for empty input" $ do
        parse P.packageSpec "" `shouldSatisfy` isLeft
    it "parses simple case correctly" $ do
        parse P.packageSpec basicInput `shouldBeOk` expected
    it "parses comma seperated correctly" $ do
        parse P.packageSpec (basicInputSep ", ") `shouldBeOk` expected
    it "parses single case correctly" $ do
        parse P.ghcPkgDump basicInput `shouldBeOk` [expected]
    it "parses multicase correctly" $ do
        parse P.ghcPkgDump (basicInput <> "\n---\n" <> basicInput) `shouldBeOk` [expected, expected]
    it "handles name-with-dash correctly" $ do
        parse P.packageSpec "name: example-dash\nexposed-modules: Data.TA" `shouldBeOk` T.PackageSpec (T.PackageInfo "example-dash") [T.ModuleName "Data.TA"]
    it "handles name with from correctly" $ do
        let rs = parse P.packageSpec "name: example\nexposed-modules: Data.TA from ghc-example-2.1:Y.TA, Data.HA"
        rs `shouldBeOk` T.PackageSpec (T.PackageInfo "example") [T.ModuleName "Data.TA", T.ModuleName "Data.HA"]
    where
        expected = T.PackageSpec (T.PackageInfo "testme") [T.ModuleName "Data.TA", T.ModuleName "Data.TB"]
        basicInput = basicInputSep " " -- TODO: QuickCheck this
        basicInputSep sep = "name: testme\nexposed-modules: Data.TA" <> sep <> "Data.TB"

moduleNameSuite = context "module name" $ do
    it "parses case with number correctly" $ do
        parse P.moduleName "X.Y1" `shouldBeOk` T.ModuleName "X.Y1"

main :: IO ()
main = hspec $ do
    describe "importDecl" importDeclSuite
    describe "comment decl" commentDeclSuite
    describe "module decl" moduleHeaderSuite
    describe "package expr" packageExprSuite
    describe "packages" TestPkgs.spec
    packageSpecSuite
    moduleNameSuite
    TestUtil.spec
    TestLib.spec











