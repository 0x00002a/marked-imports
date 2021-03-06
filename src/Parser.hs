{-# LANGUAGE OverloadedStrings #-}

module Parser where


import qualified Text.Megaparsec       as MP
import           Text.Megaparsec       ( label, (<?>), (<|>) )
import qualified Text.Megaparsec.Char  as MP
import           Text.Megaparsec.Char  ( char, hspace, letterChar, space, string )
import qualified Text.Megaparsec.Debug as MP

import           Control.Arrow         ( first )
import           Control.Monad         ( void, (>=>) )
import           Data.Maybe            ( catMaybes, fromMaybe )
import           Data.Text             ( Text )
import qualified Data.Text             as TxT
import           LUtil                 ( (><>) )
import qualified LUtil                 as Util
import qualified Text.Megaparsec       as MP
import           Text.Megaparsec.Debug ( dbg )
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

consumeLineWithNl :: Parser Text
consumeLineWithNl = uncurry (<>) . first TxT.pack <$> MP.manyTill_ MP.anySingle MP.eol

moduleQualifiers = MP.choice [text "qualified"]

spaceCare :: Parser Text
spaceCare = TxT.pack <$> MP.many (MP.try MP.newline <|> MP.spaceChar)

importDecl :: Parser T.ImportDecl
importDecl = do
    start <- importStart <> spaceCare
    modTxt <- MP.lookAhead fullMod
    MP.optional moduleQualifiers >> space
    name <- moduleName <* space
    pure (name, start <> modTxt)
    where
        importStart = text "import"
        matchEnd = text "(" <> spaceCare <> (mconcat <$> MP.many endInner) <> spaceCare <> text ")"
        endInner = MP.choice [ MP.try $ MP.lookAhead (char '(') *> matchEnd, TxT.singleton <$> MP.anySingleBut ')' ]
        explicitImportSect = spaceCare <> matchEnd
        startsOnSameLine =
            (uncurry (<>) . first TxT.pack <$> MP.manyTill_ MP.anySingle (MP.try MP.eol <|> ("" <$ MP.lookAhead (char '('))))
            <> (MP.lookAhead (char '(') *> explicitImportSect)
        multiLineExplicit = consumeLineWithNl <> explicitImportSect
        explicitImport = MP.try startsOnSameLine <|> multiLineExplicit
        fullMod = MP.try explicitImport <|> consumeLine



commentDecl :: Parser T.Comment
commentDecl = label "comment" $ MP.try singleLineCmtDecl <|> multiLineCmtDecl
    where
        singleLineCmtDecl = T.SingleLineCmt <$> (hspace *> text "--" *> consumeLine)
        multiLineCmtDecl = (\txt -> T.MultiLineCmt txt (length (TxT.lines txt))) <$> txtInsideMultiline
        txtInsideMultiline = hspace *> text "{-" *> (TxT.pack <$> MP.manyTill MP.anySingle (text "-}"))

located :: Parser a -> Parser (T.Located a)
located p =
    MP.getSourcePos >>= \pos -> T.Located (T.Pos (MP.unPos $ MP.sourceLine pos)) <$> p

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
        intoModule src (T.LineCmt cmt) mod    = mod { T.modComments = T.Located src cmt:T.modComments mod }
        intoModule src (T.LineImport imp) mod = mod { T.modImports = T.Located src imp:T.modImports mod }
        intoModule _ T.LineEmpty mod          = mod

word :: Parser Text
word = label "word" $ TxT.pack <$> MP.many MP.letterChar

someword = label "someworld" $ TxT.pack <$> MP.some MP.letterChar

packageExpr :: Parser T.PackageInfo
packageExpr = label "package expr" $
    T.PackageInfo <$> (removeDash . Util.mconcatInfix "-" <$> sepByProper someword (char '-') <* MP.optional versionExpr)
    where
        removeDash v = fromMaybe v $ TxT.stripSuffix "-" v
        versionExpr = text "-" ><> tNum ><> (mconcat <$> MP.many (text "." ><> tNum))
        tNum = TxT.singleton <$> MP.numberChar

sepByProper p c = fmap (:[]) p <> MP.many (MP.try $ c *> p)

packageSpec :: Parser T.PackageSpec
packageSpec = label "package spec" $ T.PackageSpec <$> name <*> MP.skipManyTill consumeLine_ exposes
    where
        namePrefix = text "name:" *> hspace
        nameBody = MP.many (MP.try $ word <> text "-")
        name = T.PackageInfo <$> (namePrefix *> (mconcat <$> nameBody) <> word)
        exposes = text "exposed-modules:" *> sepByProper exposesEl (MP.optional (char ',') *> space)
        exposesEl =
            space *>
            moduleName
            <* MP.optional (MP.try $
                space
                >> text "from"
                >> space
                >> packageExpr
                >> char ':'
                >> moduleName
            )
        --modEnd = char ',' *> space

ghcPkgDump :: Parser [T.PackageSpec]
ghcPkgDump = label "ghc-pkg dump" $
        sepByProper packageSpec (MP.skipManyTill MP.anySingle (text "---" <* MP.eol))



