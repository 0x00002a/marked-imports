{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Parser as P
import qualified Text.Megaparsec as MP
import qualified Types as T

parse f = MP.parse f ""


moduleTxt qualifiers name = ("import " <> qualifiers <> " " <> name, Right $ T.ModuleName name)

shouldBeOk parser check = parser `shouldBe` Right check

importDeclSuite = context "import decl suite" $ do
            it "can parse an unqualified line" $ do
                parse P.importDecl (txt "") `shouldBe` expected

            it "can parse a qualified import" $ do
                parse P.importDecl (txt "qualified") `shouldBe` expected
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
        parse P.moduleDecl "module X\nimport Y" `shouldBeOk` T.Module [(T.Located (T.Pos 2) (T.ModuleName "Y"))] []
    it "parses module header correctly" $ do
        parse P.moduleDecl "module X\n--comment\nimport Y" `shouldBeOk` T.Module [(T.Located (T.Pos 3) (T.ModuleName "Y"))] [(T.Located (T.Pos 2) (T.SingleLineCmt "comment"))]

main :: IO ()
main = hspec $ do
    describe "importDecl" importDeclSuite
    describe "comment decl" commentDeclSuite
    describe "module decl" moduleHeaderSuite
