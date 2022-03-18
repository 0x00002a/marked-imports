module Types where

import Data.Text (Text)

newtype ModuleName = ModuleName Text deriving(Show, Eq, Ord)


