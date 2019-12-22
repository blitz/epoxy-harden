module Main where

import qualified Data.ByteString        as B
import           Data.Semigroup         ((<>))
import           Options.Applicative

import           ApplicationDescription
import           EpoxyMain
import           MachineDescription

data Arguments = Arguments
  { machineJsonFile     :: FilePath,
    applicationJsonFile :: FilePath,
    kernelTemplateFile  :: FilePath,
    outputFile          :: FilePath }

doEpoxy :: Arguments -> IO ()
doEpoxy args = do
  elf <- parseElfFile (kernelTemplateFile args)
  machineDesc <- parseMachineDescription (machineJsonFile args)
  appDesc <-  parseApplicationDescription (applicationJsonFile args)
  processes <- mapM (parseElfFile . binary) (processes appDesc)
  B.writeFile (outputFile args) (generateBootImage machineDesc elf processes)
  putStrLn "Done!"

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description JSON file")
  <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description JSON file")
  <*> strOption (long "kernel" <> metavar "KERNEL" <> help "The kernel ELF file")
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "The bootable output ELF file")

main :: IO ()
main = doEpoxy =<< execParser opts
  where opts = info (arguments <**> helper) (fullDesc <> progDesc "Build a bootable Epoxy system image")
