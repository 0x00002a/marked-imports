{-# LANGUAGE OverloadedStrings #-}
module Args (
    exec
  , AppFlag(..)
  , AppOpts(..)
)
where

import           Control.Applicative ( many, (<**>), some )
import           Data.Text           ( Text )
import qualified Data.Text           as TxT
import           Options.Applicative ( flag', help, long, metavar, short, strArgument, value, (<|>), flag, option, auto )
import qualified Options.Applicative as ARG


data AppFlag = FlagInplace | FlagStripComments deriving(Eq)
data AppOpts = AppOpts { appInput :: ![Text], appFlags :: ![AppFlag], appWhitespace :: Int } deriving (Eq)

parseAppFlag :: ARG.Parser AppFlag
parseAppFlag = flag' FlagInplace (short 'i' <> help "modify in place") <|> flag' FlagStripComments (short 's' <> help "strip comments added by this tool" <> long "strip")

txtArgument :: ARG.Mod ARG.ArgumentFields String -> ARG.Parser Text
txtArgument = fmap TxT.pack . strArgument

parseAppOpts :: ARG.Parser AppOpts
parseAppOpts = AppOpts <$>
    (
    some (txtArgument (metavar "INPUTS" <> help "Input files")) 
    <|> ((:[]) <$> txtArgument (
        metavar "INPUT"
        <> help "Input file or - for stdin"
        <> value "-"
    )
    ))
    <*> many parseAppFlag
    <*> option auto (long "whitespace" <> help "lines of whitespace to add between imports" <> value 1)

exec :: IO AppOpts
exec = ARG.execParser $ ARG.info (parseAppOpts <**> ARG.helper) (ARG.header "marked-imports")




