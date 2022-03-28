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
import qualified System.Directory as DIR
import Control.Exception (catch, IOException)


main :: IO ()
main = ARG.exec >>= handleArgs

--run :: Text -> IO (Text, Text)
run name f = do
    content <- readInput name
    Lib.runT (T.SourceInfo name content) f

readInput :: Text -> IO Text
readInput "-" = TxT.hGetContents IO.stdin
readInput name = IO.withFile (TxT.unpack name) IO.ReadMode TxT.hGetContents


writeOutput :: IO.Handle -> (Text, Text) -> IO ()
writeOutput h (content, "") = TxT.hPutStr h content
writeOutput _ (_, errs) = TxT.hPutStr IO.stderr errs

maybeStrip args ast
    | isJust (find (== ARG.FlagStripComments) args) = Lib.stripPackageComments ast
    | otherwise = ast

handleArgs args
    | isJust (find (== ARG.FlagInplace) (ARG.appFlags args)) = do
            tmpDir <- DIR.getTemporaryDirectory
            (fp, file) <- IO.openTempFile tmpDir "marked-imports.inplace.tmp"
            rs <- result
            writeOutput file rs
            IO.hClose file
            catch (DIR.renameFile fp inputName) (copyOtherwise fp)
    | otherwise = do
        rs <- result
        writeOutput IO.stdout rs
    where
        result = run (ARG.appInput args) (maybeStrip (ARG.appFlags args))
        inputName = TxT.unpack $ ARG.appInput args
        copyOtherwise :: String -> IOException -> IO ()
        copyOtherwise fp _ = DIR.copyFile fp inputName >> DIR.removeFile fp
