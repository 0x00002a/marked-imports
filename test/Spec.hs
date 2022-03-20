{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Parser as P
import qualified Text.Megaparsec as MP
import qualified Types as T
import qualified TestPackages as TestPkgs
import qualified TestLUtil as TestUtil
import Util (toPretty)
import Data.Either (isLeft)

parse f = toPretty . MP.parse f ""


moduleTxt qualifiers name = ("import " <> qualifiers <> " " <> name, Right $ T.ModuleName name)

shouldBeOk parser check = parser `shouldBe` Right check

importDeclSuite = context "import decl suite" $ do
            it "can parse an unqualified line" $ do
                parse P.importDecl (txt "") `shouldBe` expected

            it "can parse a qualified import" $ do
                parse P.importDecl (txt "qualified") `shouldBe` expected
            it "fails to parse an empty" $ do
                parse P.importDecl "" `shouldSatisfy` isLeft
    where
        base qual = moduleTxt qual "X.Y"
        expected = snd $ base ""
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
    where
        expectedImport n = T.Module [(T.Located (T.Pos n) (T.ModuleName "Y"))]

packageExprSuite = context "package expression" $ do
    it "splits name properly" $
        parse (P.packageExpr) "example" `shouldBeOk` T.PackageInfo "example"
    it "splits name-version properly" $
        parse (P.packageExpr) "example-0.13.3" `shouldBeOk` T.PackageInfo "example"
    it "handles name-with-dash-version properly" $ do
        parse (P.packageExpr) "example-dash-0.1.2" `shouldBeOk` T.PackageInfo "example-dash"

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











