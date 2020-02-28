module Main where

import qualified Data.ByteString        as B
import           Data.Semigroup         ((<>))
import qualified Data.Text.IO           as T
import           Options.Applicative
import           System.FilePath.Posix


import           ApplicationDescription
import           BootImage
import           CodeGen
import           ElfReader
import           MachineDescription

data BootImageArguments = BootImageArguments
    { bootMachFile       :: FilePath
    , bootAppFile        :: FilePath
    , kernelTemplateFile :: FilePath
    , outputBootImage    :: FilePath
    }

data CodeGenArguments = CodeGenArguments
    { codeGenMachFile :: FilePath
    , codeGenAppFile  :: FilePath
    , outHpp          :: FilePath
    , outCpp          :: FilePath
    }

data Command = BootImage BootImageArguments
    | CodeGen CodeGenArguments

doBootImage :: BootImageArguments -> IO ()
doBootImage args = do
  elf <- parseElfFile (kernelTemplateFile args)
  machineDesc <- parseMachineDescription $ bootMachFile args
  appDesc     <- parseApplicationDescription (bootAppFile args)
  -- XXX This is incomplete, because we ignore everything else in the
  -- address spaces except the first ELF.
  let allProcesses = map processBinary (processes appDesc)
  B.writeFile (outputBootImage args) (generateBootImage machineDesc elf allProcesses)
  putStrLn "Done!"

doCodeGen :: CodeGenArguments -> IO ()
doCodeGen args = do
  machineDesc <- parseMachineDescription (codeGenMachFile args)
  appDesc     <- parseApplicationDescription (codeGenAppFile args)
  let generated = generateCode machineDesc appDesc $ takeFileName $ outHpp args
  T.writeFile (outCpp args) (cppContent generated)
  T.writeFile (outHpp args) (hppContent generated)

doEpoxy :: Command -> IO ()
doEpoxy (BootImage args) = doBootImage args
doEpoxy (CodeGen args)   = doCodeGen args

bootImageParser :: Parser BootImageArguments
bootImageParser = BootImageArguments
  <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description Dhall file")
  <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description Dhall file")
  <*> strOption (long "kernel" <> metavar "KERNEL" <> help "The kernel ELF file")
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "The bootable output ELF file")

codegenParser :: Parser CodeGenArguments
codegenParser = CodeGenArguments
  <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description Dhall file")
  <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description Dhall file")
  <*> strOption (long "out-hpp" <> metavar "HPP" <> help "The generated kernel state header file")
  <*> strOption (long "out-cpp" <> metavar "CPP" <> help "The generated kernel state cpp file")

cmdParser :: Parser Command
cmdParser = subparser
  (command "boot-image" (info (BootImage <$> bootImageParser <**> helper)
                         (progDesc "Build a bootable Epoxy system image")) <>
   command "codegen" (info (CodeGen <$> codegenParser <**> helper)
                       (progDesc "Generate kernel header and cpp files")))

main :: IO ()
main = doEpoxy =<< execParser opts
  where opts = info (cmdParser <**> helper) (fullDesc <> progDesc "The Epoxy Swiss Army Knife")
