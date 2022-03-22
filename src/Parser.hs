{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Text.Megaparsec       (label, (<?>), (<|>))
import qualified Text.Megaparsec       as MP
import           Text.Megaparsec.Char  (char, hspace, letterChar, space, string)
import qualified Text.Megaparsec.Char  as MP
import qualified Text.Megaparsec.Debug as MP

import           Control.Monad         (void, (>=>))
import           Data.Maybe            (catMaybes, fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as TxT
import           LUtil                 ((><>))
import qualified LUtil                 as Util
import qualified Types                 as T

type Parser a = MP.Parsec Text Text a

text :: Text -> Parser Text
text = MP.chunk


instance MP.ShowErrorComponent Text where
    showErrorComponent = show

moduleName :: Parser T.ModuleName
moduleName = label "module name" $ coerce <$> (MP.many (MP.try (moduleSect <> endingChar)) <> ((:[]) <$> moduleSect))
    where
        moduleSect = (TxT.pack <$> MP.some (MP.try letterChar <|> MP.digitChar))
        endingChar = text "."
        coerce = T.ModuleName . foldl (<>) ""

consumeLine_ :: Parser ()
consumeLine_ = MP.skipManyTill MP.anySingle (void MP.eol <|> MP.eof)

consumeLine :: Parser Text
consumeLine = TxT.pack <$> MP.manyTill MP.anySingle (void MP.eol <|> MP.eof)

moduleQualifiers = MP.choice [text "qualified"]

importDecl = (text "import" <* space) *> MP.optional moduleQualifiers *> space *> moduleName <* consumeLine_

commentDecl :: Parser T.Comment
commentDecl = label "comment" $ MP.try singleLineCmtDecl <|> multiLineCmtDecl
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
moduleDecl = label "module declaration" $ moduleStart *> parseContent
    where
        moduleStart = "module" *> hspace *> moduleName *> consumeLine_
        parseContent = foldl unpackLines mempty . catMaybes <$> MP.manyTill parseContentLine MP.eof
        parseContentLine = Just <$> MP.try (located parseLine) <|> Nothing <$ consumeLine_
        fixOrder mod = mod { T.modComments = reverse (T.modComments mod), T.modImports = reverse (T.modImports mod) }
        unpackLines mod (T.Located src v) = intoModule src v mod
        intoModule src (T.LineCmt cmt) mod = mod { T.modComments = (T.Located src cmt):T.modComments mod }
        intoModule src (T.LineImport imp) mod = mod { T.modImports = (T.Located src imp):T.modImports mod }
        intoModule _ (T.LineEmpty) mod = mod

word :: Parser Text
word = label "word" $ TxT.pack <$> MP.many MP.letterChar


packageExpr :: Parser T.PackageInfo
packageExpr = label "package expr" $
    T.PackageInfo <$> (removeDash . Util.mconcatInfix "-" <$> (MP.sepBy1 word (char '-')) <* MP.optional versionExpr)
    where
        removeDash v = fromMaybe v $ TxT.stripSuffix "-" v
        versionExpr = text "-" ><> tNum ><> (mconcat <$> MP.many (text "." ><> tNum))
        tNum = TxT.singleton <$> MP.numberChar


packageSpec :: Parser T.PackageSpec
packageSpec = label "package spec" $ T.PackageSpec <$> name <*> MP.skipManyTill consumeLine_ exposes
    where
        namePrefix = text "name:" *> hspace
        name = T.PackageInfo <$> (namePrefix *> (mconcat <$> (MP.many (MP.try $ word <> text "-"))) <> word)
        exposes = text "exposed-modules:" *> space *> MP.many (MP.try moduleName <* modEnd)
        modEnd = MP.optional (char ',') *> space

ghcPkgDump :: Parser [T.PackageSpec]
ghcPkgDump = label "ghc-pkg dump" $
        MP.many (MP.try $ packageSpec <* MP.skipManyTill MP.anySingle (text "---\n")) <> ((:[]) <$> packageSpec)



