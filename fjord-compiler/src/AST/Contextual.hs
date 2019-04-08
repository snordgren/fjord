module AST.Contextual (
  Declaration (..),
  Expression (..),
  Module (..),
  Parameter (..),
  Type (..)
) where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }

data Type = Named Int String |
  FunctionType Int Type Type
  deriving (Eq, Show)

data Expression = IntLiteral Int Integer | StringLiteral Int String | Name Int String
data Declaration = ValueDeclaration Int String [Parameter] Type Expression
data Parameter = Parameter Int String
