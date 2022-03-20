{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module LUtil where
import qualified System.Directory as SD
import qualified Data.Text as TxT
import Data.Text (Text)
import Data.List (find)
import Data.Maybe (fromJust)
import Control.Monad ((<=<))
import qualified Types as T

result :: Text -> Maybe a -> T.Result a
result v = maybe (T.err v) T.ok

mconcatInfix :: (Monoid m) => m -> [m] -> m
mconcatInfix v [] = mempty
mconcatInfix v [x] = x
mconcatInfix v (x:xs) = x <> v <> mconcatInfix v xs

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left v) = Left $ f v
mapLeft _ (Right v) = Right v

nameOfLocalPackage :: IO (Maybe Text)
nameOfLocalPackage = Just <$> (SD.getCurrentDirectory >>= findCabalFileRecur)
    where
        findCabalFileRecur :: String -> IO Text
        findCabalFileRecur dir = findCabalFile dir >>= (\case
            Nothing -> findCabalFileRecur (dir <> "../")
            Just f -> pure f)
        --findCabalFile :: String -> IO (Maybe Text)
        ext :: Text
        ext = ".cabal"
        findCabalFile =
            fmap (TxT.stripSuffix ext <=< find (TxT.isSuffixOf ext) . map TxT.pack) . SD.listDirectory
