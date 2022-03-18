module Lib
    ( run
    ) where

import qualified System.Process as SP
import Data.Text (Text, pack, unpack)
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHCEx
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHCEx
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Hs as GHC
import Data.Foldable (toList)
import qualified GHC.Parser.Lexer as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as GHCEx
import qualified GHC.Driver.Session                                  as GHC
import System.Exit (ExitCode(..))
import Data.Maybe (catMaybes)

run :: IO ()
run = case ((mapM_ putStrLn) <$> (fmap (fmap (map show . catMaybes)) $ parse "import Data.Text")) of
    Just io -> io
    Nothing -> pure ()

baseDynFlags :: GHC.DynFlags
baseDynFlags = GHC.defaultDynFlags GHCEx.fakeSettings GHCEx.fakeLlvmConfig

parse :: String -> Maybe (IO [(Maybe Text)])
parse file =
    doParse
    (sequence . map (moduleToPackage . pack . show . GHC.unLoc . GHC.ideclName) . concatMap (toList) . GHC.hsmodImports . GHC.unLoc)
    where
        doParse f = case GHCEx.parseModule file baseDynFlags of
            GHC.POk _ v -> Just $ f v
            GHC.PFailed _ -> Nothing

moduleToPackage :: Text -> IO (Maybe Text)
moduleToPackage name = handleResult <$> doMap
    where
        doMap = SP.readProcessWithExitCode "ghc-pkg" ["find-module", unpack name, "--simple-output"] ""
        handleResult (ExitSuccess, result, _) = Just $ pack result
        handleResult _ = Nothing



