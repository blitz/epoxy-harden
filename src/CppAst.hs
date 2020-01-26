{-# LANGUAGE OverloadedStrings #-}

module CppAst where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Numeric.Natural (Natural)

-- This is a sufficient subset of the C++ grammar to be able to
-- express generated code.

data CppType = Type Text | Pointer CppType | Const CppType
  deriving (Show)

data CppExpression = Identifier Text
                   | UnsignedInteger Natural
                   | AddressOf CppExpression
                   | InitializerList [CppExpression]
                   | String Text
  deriving (Show)

data ArrayLength = Exactly Int | Unspecified
  deriving (Show)

data CppStatement = Include Text
                  | Pragma Text
                  | CompoundStatement [CppStatement]
                  | AnonNamespace [CppStatement]
                  | FwdVarDeclaration CppType Text
                  | FwdArrayDeclaration CppType Text ArrayLength
                  | VarDefinition CppType Text [CppExpression]
                  | ArrayDefinition CppType Text [CppExpression]
  deriving (Show)

type CppProgram = [CppStatement]

showT :: Show a => a -> Text
showT = T.pack . show

-- We don't have arrays here, because they are hard to render. Use
-- using definitions to turn them into primitive types.
renderType :: CppType -> Text
renderType (Type name)  = name
renderType (Pointer ty) = renderType ty `T.append` " *"
renderType (Const ty)   = renderType ty `T.append` " const"

renderExpression :: CppExpression -> Text
renderExpression (Identifier name) = name
renderExpression (UnsignedInteger i) = showT i
renderExpression (AddressOf expr)  = "&(" `T.append` renderExpression expr `T.append` ")"
renderExpression (InitializerList l) = "{" `T.append` T.intercalate "," (map renderExpression l) `T.append` "}"
renderExpression (String s) = T.concat ["\"", (T.replace "\"" "\\\"" s), "\""]

-- Render the common part of a variable definition or declaration,
-- e.g. int foo[].
renderVar :: CppType -> Text -> Text
renderVar ty name = renderType ty `T.append` " " `T.append` name

renderArrayLength :: ArrayLength -> Text
renderArrayLength (Exactly l) = "[" `T.append` showT l `T.append` "]"
renderArrayLength Unspecified = "[]"

renderStatement :: CppStatement -> Text
renderStatement (Include name)    = "#include \"" `T.append` name `T.append` "\""
renderStatement (Pragma pragma)   = "#pragma " `T.append` pragma
renderStatement (CompoundStatement l) = T.intercalate "\n" $ map renderStatement l
renderStatement (AnonNamespace p) = "namespace {\n" `T.append` renderProgram p `T.append` "}"
renderStatement (FwdVarDeclaration ty name) = "extern " `T.append` renderVar ty name `T.append` ";"
renderStatement (FwdArrayDeclaration ty name l) = "extern " `T.append` renderVar ty name
  `T.append` renderArrayLength l `T.append` ";"
renderStatement (VarDefinition ty name init) =
  renderVar ty name `T.append` " " `T.append` renderExpression (InitializerList init) `T.append` ";"
renderStatement (ArrayDefinition ty name init) = renderVar ty name
  `T.append` renderArrayLength (Exactly (length init)) `T.append` " "
  `T.append` renderExpression (InitializerList init) `T.append` ";"

appendNewline :: Text -> Text
appendNewline s = s `T.append` "\n"

renderProgram :: CppProgram -> Text
renderProgram p = T.concat $ map (appendNewline . renderStatement) p
