-- A typed AST where all types have been checked and asserted valid. No position 
-- information is retained and all names are canonical. This AST is intended to 
-- be used for optimizations and other program transforms, after the program
-- has been verified correct. 
module AST.Typed (
  Module (..),
  Expression (..),
  Declaration (..),
  Type (..)
) where

import qualified AST.Contextual as C

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
data Expression = IntLiteral Integer | StringLiteral String
data Declaration = ValueDeclaration String C.Type Expression

data Type = Named String | BuiltInInt | BuiltInString
  deriving Eq

instance Show Type where
  show (Named s) = s
  show BuiltInInt = "Int"
  show BuiltInString = "String"
