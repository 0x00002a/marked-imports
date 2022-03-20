{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Lib
import qualified Types as T
import qualified System.Environment as SE
import Data.Text (Text, pack, unpack)
import Data.Bifunctor (Bifunctor(bimap))
import qualified System.IO as IO
import qualified Args as ARG
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as TxT
import qualified Data.Text.IO as TxT


main :: IO ()
main = ARG.exec >>= handleArgs

run :: Text -> IO (Text, Text)
run name = do
    content <- readInput name
    Lib.run (T.SourceInfo name content)

readInput :: Text -> IO Text
readInput "-" = TxT.hGetContents IO.stdin
readInput name = TxT.readFile (TxT.unpack name)


writeOutput :: IO.Handle -> (Text, Text) -> IO ()
writeOutput h (content, "") = TxT.hPutStr h content
writeOutput _ (_, errs) = TxT.hPutStr IO.stderr errs

handleArgs args
    | isJust (find (== ARG.FlagInplace) (ARG.appFlags args)) = do
            file <- inputFile
            rs <- result
            writeOutput file rs
            IO.hClose file
    | otherwise = do
        rs <- result
        writeOutput IO.stdout rs
    where
        result = run $ ARG.appInput args
        inputFile = IO.openFile (TxT.unpack $ ARG.appInput args) IO.ReadMode
