module Types where

import           Data.Text       ( Text )
import qualified Text.Megaparsec as MP

type ErrMsg = Text

data Pos = Pos { srcLine :: !Int } deriving(Eq, Show, Ord)
data Located a = Located Pos a deriving(Eq, Show, Ord)

data SourceInfo a = SourceInfo { sourceName :: Text, sourceContents :: a }

type Result a = Either Text a

data PackageInfo = PackageInfo { pkgName :: !Text } deriving(Show, Eq, Ord)

data PackageSpec = PackageSpec { pkgInfo :: !PackageInfo, pkgExposes :: ![ModuleName] } deriving(Show, Eq, Ord)

data ModuleName = ModuleName { modName :: !Text } deriving(Show, Eq, Ord)
type ImportDecl = (ModuleName, Text)
data Module = Module { modImports :: [Located ImportDecl], modComments :: [Located Comment] } deriving (Eq, Show)
data Comment = SingleLineCmt Text | MultiLineCmt Text Int deriving(Eq, Show, Ord)

data Line = LineCmt Comment | LineImport ImportDecl | LineEmpty

instance Semigroup Module where
    lm <> rm = Module { modImports = modImports lm <> modImports rm, modComments = modComments lm <> modComments rm }

instance Monoid Module where
    mempty = Module mempty mempty

instance Functor Located where
    fmap f (Located p v) = Located p (f v)

instance Enum Pos where
    toEnum = Pos
    fromEnum = srcLine

instance Num Pos where
    x + y = Pos $ srcLine x + srcLine y
    x * y = Pos $ srcLine x * srcLine y
    abs = Pos . abs . srcLine
    signum = Pos . signum . srcLine
    fromInteger = Pos . fromInteger
    negate = Pos . negate . srcLine


unLocated :: Located a -> a
unLocated (Located _ v) = v

posOf :: Located a -> Pos
posOf (Located p _) = p

err :: Text -> Result a
err = Left
ok :: a -> Result a
ok = Right




