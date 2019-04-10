module AST.Canonical where

import Data.List as L

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }

data Expression = IntLiteral Int Integer | 
  StringLiteral Int String | 
  Name Int String | 
  Addition Int Expression Expression |
  Apply Int Expression Expression

data Declaration = ValueDeclaration Int String [Parameter] Type Expression
data Parameter = Parameter Int String

data Type = Canonical Int String |
  BuiltInInt Int |
  BuiltInString Int |
  FunctionType Int Type Type
  deriving (Eq, Show)

data Scope = Scope { scopeBindings :: [Binding] }
data Binding = Binding { bindingName :: String, bindingType :: Type }

declarationName :: Declaration -> String
declarationName (ValueDeclaration _ name _ _ _) = name

declarationType :: Declaration -> Type
declarationType (ValueDeclaration _ _ _ t _) = t

expressionOffset :: Expression -> Int
expressionOffset (IntLiteral offset _) = offset
expressionOffset (StringLiteral offset _) = offset
expressionOffset (Name offset _) = offset
expressionOffset (Apply offset _ _) = offset

parameterName (Parameter _ n) = n
