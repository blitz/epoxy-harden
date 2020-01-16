module CodeGen (generateCode, GeneratedCode, hppContent, cppContent) where

import           Data.List
import           Text.StringTemplate
import           Text.StringTemplate.Classes

import           ApplicationDescription
import           MachineDescription

hppTemplate :: StringTemplate String
hppTemplate = newSTMP $ unlines [
  "// Automatically generated.",
  "#pragma once",
  "",
  "#include \"config_types.hpp\"",
  "#include \"process.hpp\"",
  "#include \"thread.hpp\"",
  "",
  "extern kobject kobjects[$kobjectCount$];",
  "extern thread threads[$threadCount$];",
  "extern process processes[$processCount$];",
  ""
  ]

cppTemplate :: StringTemplate String
cppTemplate = newSTMP $ unlines [
  "// Automatically generated.",
  "#include \"$headerName$\"",
  "kobject kobjects[] { $kobjectInit; separator=\", \"$ };",
  ""

  -- TODO see source_template in the config python script and complete!
  ]

data GeneratedCode = GeneratedCode
  { hppContent :: String,
    cppContent :: String }

generateHpp :: ApplicationDescription -> String
generateHpp app =
  renderf hppTemplate ("kobjectCount", kobjectCount) ("processCount", processCount) ("threadCount", processCount)
  where
    processCount = length $ processes app
    kobjectCount = length $ kobjects app

instance ToSElem KObject where
  toSElem kobj = STR $ "{kobject_type::" ++ kobjType kobj ++ "}"

sortByGid :: [KObject] -> [KObject]
sortByGid = sortBy (\a b -> compare (gid a) (gid b))

isConsecutive :: [Int] -> Bool
isConsecutive l = take (length l) [0..] == l

generateCpp :: ApplicationDescription -> String -> String
generateCpp app headerName =
  if isConsecutive $ map gid sortedKobjs
  then renderf cppTemplate ("headerName", headerName) ("kobjectInit", sortedKobjs)
  else error "Need consecutive kernel object GIDs"
  where sortedKobjs = sortByGid (kobjects app)

generateCode :: MachineDescription -> ApplicationDescription -> String -> GeneratedCode
generateCode machine app headerName = GeneratedCode (generateHpp app) (generateCpp app headerName)
