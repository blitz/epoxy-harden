{-# LANGUAGE OverloadedStrings #-}

module CodeGen (generateCode, GeneratedCode, hppContent, cppContent,
               ) where

import           Data.Elf               (elfEntry)
import           Data.List
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)

import           ApplicationDescription
import           CppAst
import           ElfReader
import           MachineDescription

data GeneratedCode = GeneratedCode
  { hppContent :: Text,
    cppContent :: Text }

generateHpp :: ApplicationDescription -> CppProgram
generateHpp app =
  [ Pragma "once"
  , Include "thread.hpp"
  , FwdArrayDeclaration (Type "thread") "threads" (Exactly $ length $ processes app)
  ]

sortByGid :: [KObject] -> [KObject]
sortByGid = sortBy (\a b -> compare (gid a) (gid b))

isConsecutive :: [Natural] -> Bool
isConsecutive l = take (length l) [0..] == l

kobjNameFromGid :: Natural -> Text
kobjNameFromGid g = "kobject_" `T.append` showT g

kobjName :: KObject -> Text
kobjName = kobjNameFromGid . gid

kobjType :: KObject -> CppType
kobjType = Type . kobjectKind

kobjFwdDecl :: KObject -> CppStatement
kobjFwdDecl k = FwdVarDeclaration (kobjType k) (kobjName k)

kobjPointerFromGid :: Natural -> CppExpression
kobjPointerFromGid = AddressOf . Identifier . kobjNameFromGid

capsetName :: KObjectImpl -> Text
capsetName p = "p" `T.append` showT (pid p) `T.append` "_capability_set"

-- kobject * const p0_capability_set[] = {&kobject_0,&kobject_1};
procCapSetDef :: KObject -> CppStatement
procCapSetDef p = ArrayDefinition kobjPointerType (capsetName (impl p)) (map kobjPointerFromGid $ processCaps p)
  where kobjPointerType = Const $ Pointer $ Type "kobject"

kobjInit :: [(Natural, Natural)] -> KObjectImpl -> [CppExpression]
kobjInit _ Exit = []
kobjInit _ KLog{prefix=p} = [String p]
kobjInit _ pr@Process{pid=pid, capabilities=c} =
  [ UnsignedInteger pid
  , Identifier (capsetName pr)]
kobjInit entryPoints Thread{process=gid} =
  [ Identifier (kobjNameFromGid gid)
  , UnsignedInteger $ lookupProcEntryPoint entryPoints gid]

kobjDef :: [(Natural, Natural)] -> KObject -> CppStatement
kobjDef entryPoints k = VarDefinition (kobjType k) (kobjName k) (kobjInit entryPoints (impl k))

lookupProcEntryPoint :: [(Natural, Natural)] -> Natural -> Natural
lookupProcEntryPoint entryPoints gid = snd $ head $ filter (\(g, _) -> g == gid) entryPoints

procEntryPoint :: KObject -> IO (Natural, Natural)
procEntryPoint KObject{gid=g, impl=Process{binary=b}} = do
  elf <- parseElfFile $ T.unpack $ b
  return (g, fromIntegral $ elfEntry $ elf)
procEntryPoint _                                      = error "no process"

statementMap :: (a -> CppStatement) -> [a] -> CppStatement
statementMap f = CompoundStatement . map f

threadArray :: ApplicationDescription -> CppStatement
threadArray a = ArrayDefinition (Const (Pointer (Type "thread"))) "threads" threadInit
  where threadInit = map (AddressOf . Identifier . kobjName) (threads a)

generateCpp :: ApplicationDescription -> Text -> IO CppProgram
generateCpp app headerName = do
  entryPoints <- mapM procEntryPoint (processes app)
  return $
    if isConsecutive $ map gid sortedKobjs
    then [ Include headerName
         , Include "kobject_all.hpp"
         , AnonNamespace
           [
             -- Forward declare all kernel objects, so they can refer
             -- to pointers to themselves without caring about
             -- initialization order.
             statementMap kobjFwdDecl sortedKobjs

             -- Define capability arrays for each process.
           , statementMap procCapSetDef procs

             -- Now construct all kernel objects.
           , statementMap (kobjDef entryPoints) sortedKobjs
           ]
           -- The scheduler needs to see all threads.
         , threadArray app
         ]
    else error "Need consecutive kernel object GIDs"
  where sortedKobjs = sortByGid (kobjects app)
        procs = processes app

generateCode :: MachineDescription -> ApplicationDescription -> String -> IO GeneratedCode
generateCode machine app headerName = do
  let hpp = renderProgram $ generateHpp app
  cpp <- renderProgram <$> generateCpp app (T.pack headerName)
  return $ GeneratedCode hpp cpp
