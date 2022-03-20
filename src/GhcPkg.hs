module GhcPkg where

import Data.Text (Text)
import qualified Data.Text as TxT
import qualified Types as T


class BackingStore a where
    insert :: a -> T.PackageSpec -> a
    lookup :: a -> T.PackageInfo -> Maybe T.PackageSpec


data Database s = Database { dbStore :: s }




