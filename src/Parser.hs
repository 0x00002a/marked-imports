{-# LANGUAGE OverloadedStrings #-}

module Parser where


import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Debug as MP
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (string, space, hspace, char, letterChar)
import qualified Text.Megaparsec.Char as MP

import qualified Data.Text as TxT
import Data.Text (Text)
import qualified Types as T
import Control.Monad (void)

type Parser a = MP.Parsec Text Text a

text :: Text -> Parser Text
text = MP.chunk


instance MP.ShowErrorComponent Text where
    showErrorComponent = show

moduleName :: Parser T.ModuleName
moduleName = coerce <$> (MP.some (MP.try (moduleSect <> endingChar)) <> ((:[]) <$> moduleSect))
    where
        moduleSect = (TxT.pack <$> MP.some letterChar)
        endingChar = text "."
        coerce = T.ModuleName . foldl (<>) ""

consumeLine_ :: Parser ()
consumeLine_ = MP.skipManyTill MP.anySingle (void MP.eol <|> MP.eof)

consumeLine :: Parser Text
consumeLine = TxT.pack <$> MP.manyTill MP.anySingle (void MP.eol <|> MP.eof)

moduleQualifiers = MP.choice [text "qualified"]

importDecl = (text "import" <* space) *> MP.optional moduleQualifiers *> space *> moduleName <* consumeLine_

commentDecl :: Parser T.Comment
commentDecl = MP.try singleLineCmtDecl <|> multiLineCmtDecl
    where
        singleLineCmtDecl = T.SingleLineCmt <$> (hspace *> text "--" *> consumeLine)
        multiLineCmtDecl = (\txt -> T.MultiLineCmt txt (TxT.length txt)) <$> txtInsideMultiline
        txtInsideMultiline = (hspace *> text "{-" *> (TxT.pack <$> (MP.manyTill MP.anySingle (text "-}"))))


moduleDecl :: Parser T.Module
moduleDecl = undefined






