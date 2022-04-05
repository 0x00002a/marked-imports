{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Packages as PKG


main :: IO ()
main = ARG.exec >>= handleArgs

data RunCtx = RunCtx { rTargetFile :: !Text, rArgs :: ARG.AppOpts }

--run :: Text -> IO (Text, Text)
run rctx ctx f = do
    let name = rTargetFile ctx
    content <- readInput name
    Lib.runWithCtxT (T.ok rctx) (T.SourceInfo name content) f


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
    | isJust (find (== ARG.FlagInplace) (ARG.appFlags args)) = runWith $ \result ctx -> do
            let inputName = TxT.unpack $ rTargetFile ctx
            tmpDir <- DIR.getTemporaryDirectory
            (fp, file) <- IO.openTempFile tmpDir "marked-imports.inplace.tmp"
            rs <- result
            writeOutput file inputName rs
            IO.hClose file
            catch (DIR.renameFile fp inputName) (copyOtherwise fp inputName)
    | otherwise = runWith $ \result ctx -> do
        let inputName = TxT.unpack $ rTargetFile ctx
        rs <- result
        writeOutput IO.stdout inputName rs
    where
        runWith f = do
            let ctxs = makeRunContexts
            ectx <- makeCtx
            either (\err -> putStrLn $ "failed to init ghc package database: " <> TxT.unpack err)
                (\ctx -> mapM_ (\c -> flip f c $ run ctx c transforms) ctxs) ectx
        makeCtx = Lib.mkPkgLookupCtx
        makeRunContexts = map makeRunContext (ARG.appInput args)
        --makeRunContext :: (forall s. PKG.MappingSource s => s) -> Text -> RunCtx
        makeRunContext file = RunCtx file args
        transforms = Lib.addLinesBeforeGroups whitespace . Lib.stripWhitespaceBetweenImports . placeLocalLowest . maybeStrip (ARG.appFlags args)

        whitespace = ARG.appWhitespace args
        copyOtherwise :: String -> String -> IOException -> IO ()
        copyOtherwise fp n _ = DIR.copyFile fp n >> DIR.removeFile fp
