module CodeGen (generateCode, GeneratedCode, hppContent, cppContent) where

import           Data.Elf                    (elfEntry)
import           Data.List
import           Data.Word                   (Word64)
import           Text.StringTemplate
import           Text.StringTemplate.Classes

import           ApplicationDescription
import           ElfReader
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
  "extern thread threads[$threadCount$];",
  "extern process processes[$processCount$];",
  ""
  ]

cppTemplate :: StringTemplate String
cppTemplate = newSTMP $ unlines [
  "// Automatically generated.",
  "#include \"$headerName$\"",
  "$kobjectInit$",
  "$capabilitySets$",
  "process processes[$processCount$] { $processInit; separator=\", \"$ };",
  "thread threads[$threadCount$] { $threadInit; separator=\", \"$ };",
  ""
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

kobjectNameFromGid :: Int -> String
kobjectNameFromGid g = "kobject_" ++ show g

kobjectName :: KObject -> String
kobjectName = kobjectNameFromGid . gid

makePointer :: String -> String
makePointer s = "&" ++ s

instance ToSElem KObject where
  toSElem kobj = STR $ "static " ++ kobjType kobj ++ "_kobject " ++ kobjectName kobj ++ ";"

newtype CapSet = CapSet Process;

capsetName :: Process -> String
capsetName p = "p" ++ (show (pid p)) ++ "_capability_set"

instance ToSElem CapSet where
  toSElem (CapSet p) = STR $ "static kobject * const " ++ capsetName p ++ "[] = {" ++ capList ++ "};\n"
    where capList :: String
          capList = intercalate "," $ map (makePointer . kobjectNameFromGid) (capabilities p)

newtype ProcessInit = ProcessInit Process

instance ToSElem ProcessInit where
  toSElem (ProcessInit p) = STR $ "{" ++ show (pid p) ++ ",{" ++ show (length (capabilities p))
                                  ++ "," ++ capsetName p ++ "}}";

sortByGid :: [KObject] -> [KObject]
sortByGid = sortBy (\a b -> compare (gid a) (gid b))

isConsecutive :: [Int] -> Bool
isConsecutive l = take (length l) [0..] == l

data ThreadInit = ThreadInit
  { threadPid        :: Int,
    threadEntryPoint :: Word64 };

instance ToSElem ThreadInit where
  toSElem ti = STR $ "{&processes[" ++ show (threadPid ti) ++ "], " ++ show (threadEntryPoint ti) ++ "ULL}"

toThreadInit :: Process -> IO ThreadInit
toThreadInit p = do
  elf <- parseElfFile $ binary p
  return $ ThreadInit (pid p) (elfEntry elf)

toThreadInits :: ApplicationDescription -> IO [ThreadInit]
toThreadInits app = mapM toThreadInit $ processes app

generateCpp :: ApplicationDescription -> String -> IO String
generateCpp app headerName = do
  threadInit <- toThreadInits app
  return $
    if isConsecutive $ map gid sortedKobjs
    then renderf cppTemplate ("headerName", headerName)
                             ("kobjectInit", sortedKobjs)
                             ("capabilitySets", map CapSet procs)
                             ("processCount", length procs)
                             ("processInit", map ProcessInit procs)
                             ("threadCount", length procs)
                             ("threadInit", threadInit)
    else error "Need consecutive kernel object GIDs"
  where sortedKobjs = sortByGid (kobjects app)
        procs = processes app

generateCode :: MachineDescription -> ApplicationDescription -> String -> IO GeneratedCode
generateCode machine app headerName = do
  let hpp = generateHpp app
  cpp <- generateCpp app headerName
  return $ GeneratedCode hpp cpp
