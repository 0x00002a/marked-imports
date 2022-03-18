{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Parser as P
import qualified Text.Megaparsec as MP
import qualified Types as T

parse f = MP.parse f ""

main :: IO ()
main = hspec $ do
    describe "importDecl" $ do
        it "can parse an unqualified line" $ do
            parse P.importDecl "import X.Y" `shouldBe` (Right $ T.ModuleName "X.Y")
        
