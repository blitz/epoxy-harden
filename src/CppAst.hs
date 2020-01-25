module CppAst where

import           Data.List (intercalate)
import           Data.Word (Word64)

-- This is a sufficient subset of the C++ grammar to be able to
-- express generated code.

data CppType = Type String | Pointer CppType | Const CppType
  deriving (Show)

data CppExpression = Identifier String
                   | UnsignedInteger Word64
                   | AddressOf CppExpression
                   | InitializerList [CppExpression]
  deriving (Show)

data ArrayLength = Exactly Int | Unspecified
  deriving (Show)

data CppStatement = Include String
                  | Pragma String
                  | CompoundStatement [CppStatement]
                  | AnonNamespace [CppStatement]
                  | FwdVarDeclaration CppType String
                  | FwdArrayDeclaration CppType String ArrayLength
                  | VarDefinition CppType String [CppExpression]
                  | ArrayDefinition CppType String [CppExpression]
  deriving (Show)

type CppProgram = [CppStatement]

-- We don't have arrays here, because they are hard to render. Use
-- using definitions to turn them into primitive types.
renderType :: CppType -> String
renderType (Type name)  = name
renderType (Pointer ty) = renderType ty ++ " *"
renderType (Const ty)   = renderType ty ++ " const"

renderExpression :: CppExpression -> String
renderExpression (Identifier name) = name
renderExpression (UnsignedInteger i) = show i
renderExpression (AddressOf expr)  = "&(" ++ renderExpression expr ++ ")"
renderExpression (InitializerList l) = "{" ++ intercalate "," (map renderExpression l) ++ "}"

-- Render the common part of a variable definition or declaration,
-- e.g. int foo[].
renderVar :: CppType -> String -> String
renderVar ty name = renderType ty ++ " " ++ name

renderArrayLength :: ArrayLength -> String
renderArrayLength (Exactly l) = "[" ++ show l ++ "]"
renderArrayLength Unspecified = "[]"

renderStatement :: CppStatement -> String
renderStatement (Include name)    = "#include \"" ++ name ++ "\""
renderStatement (Pragma pragma)   = "#pragma " ++ pragma
renderStatement (CompoundStatement l) = intercalate "\n" $ map renderStatement l
renderStatement (AnonNamespace p) = "namespace {\n" ++ renderProgram p ++ "}"
renderStatement (FwdVarDeclaration ty name) = "extern " ++ renderVar ty name ++ ";"
renderStatement (FwdArrayDeclaration ty name l) = "extern " ++ renderVar ty name ++ renderArrayLength l ++ ";"
renderStatement (VarDefinition ty name init) =
  renderVar ty name ++ " " ++ renderExpression (InitializerList init) ++ ";"
renderStatement (ArrayDefinition ty name init) = renderVar ty name ++ renderArrayLength (Exactly (length init)) ++ " "
  ++ renderExpression (InitializerList init) ++ ";"

appendNewline :: String -> String
appendNewline s = s ++ "\n"

renderProgram :: CppProgram -> String
renderProgram = concatMap $ appendNewline . renderStatement
