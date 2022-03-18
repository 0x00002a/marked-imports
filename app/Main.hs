module Main where

import Lib
import qualified Types as T
import qualified System.Environment as SE
import Data.Text (pack, unpack)

main :: IO ()
main = do
    args <- SE.getArgs
    content <- readFile (head args)
    commented <- unpack <$> run (T.SourceInfo (pack (head args)) (pack content))
    putStrLn commented
