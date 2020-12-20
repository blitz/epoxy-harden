module Main where

import qualified Data.ByteString     as B
import qualified Data.Text.IO        as T
import           DhallUtils          (prettyPrint, toDhall)
import           DtbConvert          as D
import           Options.Applicative
import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (stderr)

data CmdlineArgs = CmdlineArgs
  { inputFile :: FilePath
  }

cmdlineArgs :: Parser CmdlineArgs
cmdlineArgs = CmdlineArgs <$> argument str (metavar "DTB-FILE")

opts :: ParserInfo CmdlineArgs
opts = info (cmdlineArgs <**> helper)
  ( fullDesc
    <> progDesc "Reads a device tree file and outputs a machine description to stdout."
    <> header "epoxy-dtb - device tree conversion tool")

main :: IO ()
main = do
  options <- execParser opts
  fileContent <- B.readFile $ inputFile options
  case D.convert fileContent of
    Left errMsg -> do
      T.hPutStr stderr errMsg
      exitFailure
    Right mdesc -> do
      T.putStr $ prettyPrint $ toDhall mdesc
      exitSuccess
