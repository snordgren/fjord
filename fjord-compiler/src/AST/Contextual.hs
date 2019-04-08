module AST.Contextual (
  Module (..),
  Type (..),
  Expression (..),
  Declaration (..),
  expressionOffset
) where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
data Type = Named Int String deriving (Eq, Show)
data Expression = IntLiteral Int Integer | StringLiteral Int String
data Declaration = ValueDeclaration Int String Type Expression

expressionOffset :: Expression -> Int
expressionOffset (IntLiteral offset _) = offset
expressionOffset (StringLiteral offset _) = offset
