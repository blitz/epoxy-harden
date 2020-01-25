module CodeGen (generateCode, GeneratedCode, hppContent, cppContent,
               ) where

import           Data.Elf               (elfEntry)
import           Data.List
import           Data.Word              (Word64)

import           ApplicationDescription
import           CppAst
import           ElfReader
import           MachineDescription

-- exampleProgram :: CppProgram
-- exampleProgram = [Include "state.hpp",
--                    AnonNamespace [
--                      FwdVarDeclaration (Type "exit_kobject") "kobject_0",
--                      ArrayDefinition (Type "kobject_array") "p0_capability_set"
--                      [AddressOf (Identifier "kobject_0")]
--                      ],
--                    VarDefinition (Type "thread_array") "threads" (InitializerList [])
--                  ]

processEntry :: Process -> IO Word64
processEntry p = do
  elf <- parseElfFile $ binary p
  return $ elfEntry elf

data GeneratedCode = GeneratedCode
  { hppContent :: String,
    cppContent :: String }

generateHpp :: ApplicationDescription -> CppProgram
generateHpp app =
  [ Pragma "once"
  , Include "thread.hpp"
  , FwdArrayDeclaration (Type "thread") "threads" (Exactly $ length $ processes app)
  ]

sortByGid :: [KObject] -> [KObject]
sortByGid = sortBy (\a b -> compare (gid a) (gid b))

isConsecutive :: [Int] -> Bool
isConsecutive l = take (length l) [0..] == l

kobjNameFromGid :: Int -> String
kobjNameFromGid g = "kobject_" ++ show g

kobjName :: KObject -> String
kobjName = kobjNameFromGid . gid

kobjFullType :: KObject -> CppType
kobjFullType k = Type ((kobjType k) ++ "_kobject")

kobjFwdDecl :: KObject -> CppStatement
kobjFwdDecl k = FwdVarDeclaration (kobjFullType k) (kobjName k)

kobjPointerFromGid :: Int -> CppExpression
kobjPointerFromGid = AddressOf . Identifier . kobjNameFromGid

capsetName :: Process -> String
capsetName p = "p" ++ show (pid p) ++ "_capability_set"

-- kobject * const p0_capability_set[] = {&kobject_0,&kobject_1};
procCapSetDef :: Process -> CppStatement
procCapSetDef p = ArrayDefinition kobjPointerType (capsetName p) (map kobjPointerFromGid $ capabilities p)
  where kobjPointerType = Const $ Pointer $ Type "kobject"

procName :: Process -> String
procName p = "process_" ++ show (pid p)

kobjDef :: KObject -> CppStatement
kobjDef k = VarDefinition (kobjFullType k) (kobjName k) []

procDef :: Process -> CppStatement
procDef p = VarDefinition (Type "process") (procName p)
            [ UnsignedInteger (fromIntegral (pid p))
            , InitializerList
              [ UnsignedInteger (fromIntegral (length (capabilities p)))
              , Identifier (capsetName p)
              ]]

threadInitExprs :: Process -> IO CppExpression
threadInitExprs p = do
  entry <- processEntry p
  return $ InitializerList [ AddressOf (Identifier (procName p))
                           , UnsignedInteger entry]

statementMap :: (a -> CppStatement) -> [a] -> CppStatement
statementMap f = CompoundStatement . map f

generateCpp :: ApplicationDescription -> String -> IO CppProgram
generateCpp app headerName = do
  threadInits <- mapM threadInitExprs procs
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
           , statementMap kobjDef sortedKobjs

             -- Construct all processes.
           , statementMap procDef procs
           ]
         , ArrayDefinition (Type "thread") "threads" threadInits
         ]
    else error "Need consecutive kernel object GIDs"
  where sortedKobjs = sortByGid (kobjects app)
        procs = processes app

generateCode :: MachineDescription -> ApplicationDescription -> String -> IO GeneratedCode
generateCode machine app headerName = do
  let hpp = renderProgram $ generateHpp app
  cpp <- renderProgram <$> generateCpp app headerName
  return $ GeneratedCode hpp cpp
