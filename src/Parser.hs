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
import Control.Monad (void, (>=>))
import Data.Maybe (catMaybes, fromMaybe)
import qualified LUtil as Util

type Parser a = MP.Parsec Text Text a

text :: Text -> Parser Text
text = MP.chunk


instance MP.ShowErrorComponent Text where
    showErrorComponent = show

moduleName :: Parser T.ModuleName
moduleName = coerce <$> (MP.many (MP.try (moduleSect <> endingChar)) <> ((:[]) <$> moduleSect))
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
        multiLineCmtDecl = (\txt -> T.MultiLineCmt txt (length (TxT.lines txt))) <$> txtInsideMultiline
        txtInsideMultiline = (hspace *> text "{-" *> (TxT.pack <$> (MP.manyTill MP.anySingle (text "-}"))))

located :: Parser a -> Parser (T.Located a)
located p =
    MP.getSourcePos >>= \pos -> (T.Located (T.Pos (MP.unPos $ MP.sourceLine pos))) <$> p

parseLine :: Parser T.Line
parseLine = MP.choice $ map MP.try [
        T.LineCmt <$> commentDecl
      , T.LineImport <$> importDecl
    ]

parseFile :: Parser T.Module
parseFile = MP.skipManyTill consumeLine_ moduleDecl

moduleDecl :: Parser T.Module
moduleDecl = moduleStart *> parseContent
    where
        moduleStart = "module" *> hspace *> moduleName *> consumeLine_
        parseContent = foldl unpackLines mempty <$> do
            s <- MP.skipManyTill consumeLine_ (located parseLine)
            (s:) <$> MP.many (located parseLine)

        unpackLines mod (T.Located src v) = intoModule src v mod
        intoModule src (T.LineCmt cmt) mod = mod { T.modComments = (T.Located src cmt):T.modComments mod }
        intoModule src (T.LineImport imp) mod = mod { T.modImports = (T.Located src imp):T.modImports mod }
        intoModule _ (T.LineEmpty) mod = mod

word :: Parser Text
word = TxT.pack <$> MP.many MP.letterChar

(><>) :: (Applicative m, Semigroup a) => m a -> m a -> m a
l ><> r = (<>) <$> l <*> r


packageExpr :: Parser T.PackageInfo
packageExpr = T.PackageInfo <$> (removeDash . Util.mconcatInfix "-" <$> (MP.sepBy1 word (char '-')) <* MP.optional versionExpr)
    where
        removeDash v = fromMaybe v $ TxT.stripSuffix "-" v
        versionExpr = text "-" ><> tNum ><> (mconcat <$> MP.many (text "." ><> tNum))
        tNum = TxT.singleton <$> MP.numberChar


packageSpec :: Parser T.PackageSpec
packageSpec = T.PackageSpec <$> name <*> MP.skipManyTill consumeLine_ exposes
    where
        name = T.PackageInfo <$> (text "name:" *> hspace *> word)
        exposes = text "exposed-modules:" *> space *> MP.sepBy moduleName space



