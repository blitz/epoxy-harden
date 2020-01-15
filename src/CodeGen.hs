module CodeGen (generateCode, GeneratedCode, hppContent, cppContent) where

import           ApplicationDescription
import           MachineDescription

data GeneratedCode = GeneratedCode
  { hppContent :: String,
    cppContent :: String }

generateCode :: MachineDescription -> ApplicationDescription -> String -> GeneratedCode
generateCode machine app headerName = GeneratedCode "" ""
