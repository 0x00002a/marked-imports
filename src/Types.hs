module Types where

import Data.Text (Text)
import qualified Text.Megaparsec as MP

data Pos = Pos { srcLine :: !Int } deriving(Eq, Show, Ord)
data Located a = Located Pos a deriving(Eq, Show, Ord)

data SourceInfo a = SourceInfo { sourceName :: Text, sourceContents :: a }

type Result a = Either Text a

data PackageInfo = PackageInfo { pkgName :: !Text, pkgVersion :: !Text } deriving(Show, Eq, Ord)

data ModuleName = ModuleName { modName :: !Text } deriving(Show, Eq, Ord)
data Module = Module { modImports :: [Located ModuleName], modComments :: [Located Comment] } deriving (Eq, Show)
data Comment = SingleLineCmt Text | MultiLineCmt Text Int deriving(Eq, Show, Ord)

data Line = LineCmt Comment | LineImport ModuleName | LineEmpty

instance Semigroup Module where
    lm <> rm = Module { modImports = modImports lm <> modImports rm, modComments = modComments lm <> modComments rm }

instance Monoid Module where
    mempty = Module mempty mempty

instance Functor Located where
    fmap f (Located p v) = Located p (f v)


unLocated :: Located a -> a
unLocated (Located _ v) = v

posOf :: Located a -> Pos
posOf (Located p _) = p


