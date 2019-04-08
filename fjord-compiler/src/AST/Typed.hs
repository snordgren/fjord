-- A typed AST where all types have been checked and asserted valid. No position 
-- information is retained and all names are canonical. This AST is intended to 
-- be used for optimizations and other program transforms, after the program
-- has been verified correct. 
module AST.Typed (
  Declaration (..),
  Expression (..),
  Module (..),
  Parameter (..),
  Type (..)
) where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
data Expression = IntLiteral Integer | StringLiteral String | Name String
data Declaration = ValueDeclaration String [Parameter] Type Expression
data Parameter = Parameter { parameterName :: String, parameterType :: Type }

data Type = TypeName String | BuiltInInt | BuiltInString | FunctionType Type Type
  deriving Eq

instance Show Type where
  show (TypeName s) = s
  show BuiltInInt = "BuiltIn.Int"
  show BuiltInString = "BuiltIn.String"
  show (FunctionType p r) = (show p) ++ " -> " ++ (show r)
