{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module LUtil where
import qualified System.Directory as SD
import qualified Data.Text as TxT
import Data.Text (Text)
import Data.List (find)
import Data.Maybe (fromJust)
import Control.Monad ((<=<), join)
import qualified Types as T
import System.FilePath as FP
import Control.Arrow (Arrow(second))


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
nameOfLocalPackage = nameOfLocalPackage' SD.listDirectory

nameOfLocalPackage' :: (FilePath -> IO [FilePath]) -> IO (Maybe Text)
nameOfLocalPackage' ls = SD.getCurrentDirectory >>= findCabalFileRecur
    where
        findCabalFileRecur :: String -> IO (Maybe Text)
        findCabalFileRecur dir = findCabalFile dir >>= (\case
            Nothing -> join <$> sequence (findCabalFileRecur <$> nextLevel dir)
            Just f -> pure $ pure f)
        nextLevel dir | FP.takeDirectory dir == dir = Nothing
                      | otherwise = Just $ FP.takeDirectory dir
        ext :: Text
        ext = ".cabal"
        findCabalFile =
            fmap (TxT.stripSuffix ext <=< find (TxT.isSuffixOf ext) . map TxT.pack) . ls

foldlWithIndex :: (Foldable f, Num a) => (b -> a -> c -> b) -> b -> f c -> b
foldlWithIndex f b = snd . foldl (\(n, xs) x -> (n + 1, f xs n x)) (1, b)

(><>) :: (Applicative m, Semigroup a) => m a -> m a -> m a
l ><> r = (<>) <$> l <*> r
