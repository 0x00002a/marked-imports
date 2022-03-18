{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    ) where

import qualified System.Process as SP
import Data.Text (Text, pack, unpack)
import Data.Foldable (toList)
import System.Exit (ExitCode(..))
import Data.Maybe (catMaybes)
import qualified Text.Megaparsec as MP
import qualified Parser as P

run :: IO ()
run = case MP.parse P.importDecl "" "import myt Data.Text" of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right a -> putStrLn $ show a
