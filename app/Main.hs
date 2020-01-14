module Main where

import qualified Data.ByteString        as B
import           Data.Semigroup         ((<>))
import           Options.Applicative

import           ApplicationDescription
import           BootImage
import           MachineDescription

data BootImageArguments = BootImageArguments
  { machineJsonFile     :: FilePath,
    applicationJsonFile :: FilePath,
    kernelTemplateFile  :: FilePath,
    outputFile          :: FilePath }

doEpoxy :: BootImageArguments -> IO ()
doEpoxy args = do
  elf <- parseElfFile (kernelTemplateFile args)
  machineDesc <- parseMachineDescription (machineJsonFile args)
  appDesc <-  parseApplicationDescription (applicationJsonFile args)
  processes <- mapM (parseElfFile . binary) (processes appDesc)
  B.writeFile (outputFile args) (generateBootImage machineDesc elf processes)
  putStrLn "Done!"

bootImageParser :: Parser BootImageArguments
bootImageParser = BootImageArguments
  <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description JSON file")
  <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description JSON file")
  <*> strOption (long "kernel" <> metavar "KERNEL" <> help "The kernel ELF file")
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "The bootable output ELF file")

cmdParser :: Parser BootImageArguments
cmdParser = subparser
  (command "boot-image" (info (bootImageParser <**> helper) (progDesc "Build a bootable Epoxy system image")))

main :: IO ()
main = doEpoxy =<< execParser opts
  where opts = info (cmdParser <**> helper) (fullDesc <> progDesc "The Epoxy Swiss Army Knife")
