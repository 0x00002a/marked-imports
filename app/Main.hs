{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import qualified Args               as ARG
import           Control.Exception  ( IOException, catch )
import           Data.Bifunctor     ( Bifunctor (bimap) )
import           Data.List          ( find )
import           Data.Maybe         ( isJust, fromMaybe )
import           Data.Text          ( Text, pack, unpack )
import qualified Data.Text          as TxT
import qualified Data.Text.IO       as TxT
import qualified Lib
import qualified System.Directory   as DIR
import qualified System.Environment as SE
import qualified System.IO          as IO
import qualified Types              as T


main :: IO ()
main = ARG.exec >>= handleArgs

--run :: Text -> IO (Text, Text)
run name f = do
    content <- readInput name
    Lib.runT (T.SourceInfo name content) f

readInput :: Text -> IO Text
readInput "-"  = TxT.hGetContents IO.stdin
readInput name = IO.withFile (TxT.unpack name) IO.ReadMode TxT.hGetContents


writeOutput :: IO.Handle -> FilePath -> (Text, Text) -> IO ()
writeOutput h _ (content, "") = TxT.hPutStr h content
writeOutput _ file (_, errs)  = TxT.hPutStr IO.stderr $ TxT.pack file <> ": " <> errs

maybeStrip args ast
    | isJust (find (== ARG.FlagStripComments) args) = Lib.stripPackageComments ast
    | otherwise = ast

placeLocalLowest ast = Lib.sortImportsOn (\pkg -> if (T.pkgName pkg) == "local"
                        then (length ast)
                        else 0
                        ) ast

handleArgs args
    | isJust (find (== ARG.FlagInplace) (ARG.appFlags args)) = do
            tmpDir <- DIR.getTemporaryDirectory
            (fp, file) <- IO.openTempFile tmpDir "marked-imports.inplace.tmp"
            rs <- result
            writeOutput file inputName rs
            IO.hClose file
            catch (DIR.renameFile fp inputName) (copyOtherwise fp)
    | otherwise = do
        rs <- result
        writeOutput IO.stdout inputName rs
    where
        result = run (ARG.appInput args) (Lib.addLinesBeforeGroups whitespace . Lib.stripWhitespaceBetweenImports . placeLocalLowest . maybeStrip (ARG.appFlags args))
        inputName = TxT.unpack $ ARG.appInput args
        whitespace = ARG.appWhitespace args
        copyOtherwise :: String -> IOException -> IO ()
        copyOtherwise fp _ = DIR.copyFile fp inputName >> DIR.removeFile fp
