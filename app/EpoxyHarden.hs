module Main where

import           ApplicationDescriptionParser
import           BootImage
import           CodeGen
import qualified Data.ByteString              as B
import qualified Data.Text.IO                 as T
import           ElfReader
import qualified GHC.IO.Encoding
import           MachineDescription
import           Options.Applicative
import           System.FilePath.Posix
import qualified System.IO

data BootImageArguments = BootImageArguments
  { bootMachFile       :: FilePath,
    bootAppFile        :: FilePath,
    targetArch         :: String,
    kernelTemplateFile :: FilePath,
    outputFormat       :: String,
    outputBootImage    :: FilePath,
    bootBinaryRoot     :: FilePath
  }

data CodeGenArguments = CodeGenArguments
  { codeGenMachFile   :: FilePath,
    codeGenAppFile    :: FilePath,
    outHpp            :: FilePath,
    outCpp            :: FilePath,
    codeGenBinaryRoot :: FilePath
  }

data Command
  = BootImage BootImageArguments
  | CodeGen CodeGenArguments

doBootImage :: BootImageArguments -> IO ()
doBootImage args = do
  elf <- parseElfFile (kernelTemplateFile args)
  machineDesc <- parseMachineDescription $ bootMachFile args
  appDesc <- parseApplicationDescription (bootBinaryRoot args) (bootAppFile args)
  let bootImage = generateBootImage $
        BootImageConfig machineDesc appDesc (targetArch args) elf (outputFormat args)
  B.writeFile (outputBootImage args) bootImage
  putStrLn "Done!"

doCodeGen :: CodeGenArguments -> IO ()
doCodeGen args = do
  machineDesc <- parseMachineDescription (codeGenMachFile args)
  appDesc <- parseApplicationDescription (codeGenBinaryRoot args) (codeGenAppFile args)
  let generated = generateCode machineDesc appDesc $ takeFileName $ outHpp args
  T.writeFile (outCpp args) (cppContent generated)
  T.writeFile (outHpp args) (hppContent generated)

doEpoxy :: Command -> IO ()
doEpoxy (BootImage args) = doBootImage args
doEpoxy (CodeGen args)   = doCodeGen args

bootImageParser :: Parser BootImageArguments
bootImageParser =
  BootImageArguments
    <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description Dhall file")
    <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description Dhall file")
    <*> strOption (long "target-arch" <> metavar "ARCH" <> help "The target architecture"
                  <> value "riscv-sv39" <> showDefault)
    <*> strOption (long "kernel" <> metavar "KERNEL" <> help "The kernel ELF file")
    <*> strOption (long "output-format" <> metavar "FORMAT" <> help "The format used for the output file"
                   <> value "riscv-elf64" <> showDefault)
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "The output boot image")
    <*> strOption (long "binary-root" <> metavar "ROOT" <> help "The directory relative to which user binaries are looked up"
                  <> value "./" <> showDefault)

codegenParser :: Parser CodeGenArguments
codegenParser =
  CodeGenArguments
    <$> strOption (long "machine" <> metavar "MACHINE" <> help "The machine description Dhall file")
    <*> strOption (long "application" <> metavar "APPLICATION" <> help "The application description Dhall file")
    <*> strOption (long "out-hpp" <> metavar "HPP" <> help "The generated kernel state header file")
    <*> strOption (long "out-cpp" <> metavar "CPP" <> help "The generated kernel state cpp file")
    <*> strOption (long "binary-root" <> metavar "ROOT" <> help "The directory relative to which user binaries are looked up"
                  <> value "./" <> showDefault)

cmdParser :: Parser Command
cmdParser =
  subparser
    ( command
        "boot-image"
        ( info
            (BootImage <$> bootImageParser <**> helper)
            (progDesc "Build a bootable Epoxy system image")
        )
        <> command
          "codegen"
          ( info
              (CodeGen <$> codegenParser <**> helper)
              (progDesc "Generate kernel header and cpp files")
          )
    )

main :: IO ()
main = do
  -- Setting the encoding manually is required to make Dhall recognize Unicode
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  doEpoxy =<< execParser opts
  where
    opts = info (cmdParser <**> helper) (fullDesc <> progDesc "The Epoxy Swiss Army Knife")
