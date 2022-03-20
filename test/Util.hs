{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (toPretty) where
import qualified Text.Megaparsec as MP


newtype PrettyError s e = PrettyError (MP.ParseErrorBundle s e)

instance (Eq s, Eq (MP.Token s), Eq e) => Eq (PrettyError s e) where
    l == r = unPretty l == unPretty r

instance (MP.VisualStream s, MP.TraversableStream s, MP.ShowErrorComponent e) => Show (PrettyError s e) where
    show (PrettyError e) = MP.errorBundlePretty e

unPretty (PrettyError e) = e

toPretty :: Either (MP.ParseErrorBundle s e) a -> Either (PrettyError s e) a
toPretty (Right v) = Right v
toPretty (Left e) = Left $ PrettyError e


