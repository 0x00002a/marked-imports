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

main :: IO ()
main = hspec $ do
    describe "importDecl" importDeclSuite
    describe "comment decl" commentDeclSuite