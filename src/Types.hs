module Types where

import Data.Text (Text)
import qualified Text.Megaparsec as MP

data Located a = Located MP.SourcePos a deriving(Eq, Show, Ord)

newtype ModuleName = ModuleName Text deriving(Show, Eq, Ord)
newtype Module = Module { modImports :: [Located ModuleName] } deriving (Eq)
data Comment = SingleLineCmt Text | MultiLineCmt Text Int deriving(Eq, Show, Ord)


instance Functor Located where
    fmap f (Located p v) = Located p (f v)



