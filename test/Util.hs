{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (toPretty, mkDummyCtx) where
import qualified Text.Megaparsec as MP
import Data.Text (Text)
import qualified Packages as PKG
import qualified GhcPkg as GPKG
import qualified Types as T

newtype ConstSource = ConstSource Text
instance PKG.MappingSource ConstSource where
    providerOfModule (ConstSource s) _ = pure $ pure (T.PackageInfo s)

mkDummyCtx :: Text -> T.Result ConstSource
mkDummyCtx txt = pure $ ConstSource txt

newtype PrettyError s e = PrettyError (MP.ParseErrorBundle s e)

instance (Eq s, Eq (MP.Token s), Eq e) => Eq (PrettyError s e) where
    l == r = unPretty l == unPretty r

instance (MP.VisualStream s, MP.TraversableStream s, MP.ShowErrorComponent e) => Show (PrettyError s e) where
    show (PrettyError e) = MP.errorBundlePretty e

unPretty (PrettyError e) = e

toPretty :: Either (MP.ParseErrorBundle s e) a -> Either (PrettyError s e) a
toPretty (Right v) = Right v
toPretty (Left e) = Left $ PrettyError e


