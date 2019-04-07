module AST.Contextual (
  Module (..),
  Type,
  Expression (..),
  Declaration (..)
) where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
type Type = String
data Expression = IntLiteral Integer | StringLiteral String
data Declaration = ValueDeclaration String Expression
