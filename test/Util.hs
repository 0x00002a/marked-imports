{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (toPretty, mkFuncCtx, mkDummyCtx) where
import qualified Text.Megaparsec as MP
import Data.Text (Text)
import qualified Packages as PKG
import qualified GhcPkg as GPKG
import qualified Types as T

newtype ConstSource = ConstSource Text
newtype FuncSource = FuncSource (Text -> Text)

instance PKG.MappingSource ConstSource where
    providerOfModule (ConstSource s) _ = pure $ pure (T.PackageInfo s)

instance PKG.MappingSource FuncSource where
    providerOfModule (FuncSource s) n = pure $ pure (T.PackageInfo (s (T.modName n)))

mkDummyCtx :: Text -> T.Result ConstSource
mkDummyCtx txt = pure $ ConstSource txt

mkFuncCtx :: (Text -> Text) -> T.Result FuncSource
mkFuncCtx = pure . FuncSource

newtype PrettyError s e = PrettyError (MP.ParseErrorBundle s e)

instance (Eq s, Eq (MP.Token s), Eq e) => Eq (PrettyError s e) where
    l == r = unPretty l == unPretty r

instance (MP.VisualStream s, MP.TraversableStream s, MP.ShowErrorComponent e) => Show (PrettyError s e) where
    show (PrettyError e) = MP.errorBundlePretty e

unPretty (PrettyError e) = e

toPretty :: Either (MP.ParseErrorBundle s e) a -> Either (PrettyError s e) a
toPretty (Right v) = Right v
toPretty (Left e) = Left $ PrettyError e


