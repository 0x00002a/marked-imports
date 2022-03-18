{-# LANGUAGE OverloadedStrings #-}

module Parser where


import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char (string, space, char, letterChar)
import qualified Text.Megaparsec.Char as MP

import qualified Data.Text as TxT
import Data.Text (Text)
import qualified Types as T
import Control.Monad (void)

type Parser a = MP.Parsec Text Text a

text :: Text -> Parser Text
text = MP.chunk


moduleName :: Parser T.ModuleName
moduleName = coerce <$> MP.manyTill (text " ") moduleSect
    where
        moduleSect = (TxT.pack <$> MP.some letterChar) <> text "."
        coerce = T.ModuleName . foldl (<>) ""

consumeLine :: Parser Text
consumeLine = MP.skipManyTill MP.anySingle MP.eol

moduleQualifiers = MP.choice [text "qualified"]

importDecl = (text "import" <* space) *> MP.optional moduleQualifiers *> moduleName <* consumeLine






