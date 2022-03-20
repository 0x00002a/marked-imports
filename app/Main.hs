{-# LANGUAGE TupleSections #-}
module Main where

import Lib
import qualified Types as T
import qualified System.Environment as SE
import Data.Text (pack, unpack)
import Data.Bifunctor (Bifunctor(bimap))
import qualified System.IO as IO

main :: IO ()
main = do
    args <- SE.getArgs
    content <- readFile (head args)
    commented <- bimap unpack unpack <$> run (T.SourceInfo (pack (head args)) (pack content))
    mapM_ (uncurry IO.hPutStrLn) $ bimap (IO.stdout,) (IO.stderr,) commented
