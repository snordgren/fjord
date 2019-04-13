module AST.Resolved where

import Data.List as L

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }

data Expression 
  = IntLiteral Int Integer 
  | StringLiteral Int String 
  | Name Int String 
  | Addition Int Expression Expression 
  | Apply Int Expression Expression
  | Case Int Expression [Pattern]
  | Lambda Int String Expression
  | RecordUpdate Int Expression [FieldUpdate]

data FieldUpdate = FieldUpdate 
  {
    fieldUpdateOffset :: Int,
    fieldUpdateName :: String,
    fieldUpdateExpression :: Expression
  }

data Pattern = Pattern 
  {
    patternOffset :: Int,
    patternConstructor :: String,
    patternValues :: [String],
    patternExpression :: Expression
  }

data Declaration 
  = EnumDeclaration Int String [EnumConstructor]
  | RecordDeclaration Int String [RecordField]
  | ValueDeclaration Int String [Parameter] Type Expression

data Parameter = Parameter Int String

data EnumConstructor = EnumConstructor 
  {
    enumConstructorOffset :: Int,
    enumConstructorName :: String,
    enumConstructorType :: Type
  }

data RecordField = RecordField 
  { 
    recordFieldOffset :: Int,
    recordFieldName :: String,
    recordFieldType :: Type
  }

data Type = Canonical Int String |
  BuiltInInt Int |
  BuiltInString Int |
  FunctionType Int Type Type
  deriving (Eq, Show)

data Scope = Scope { scopeBindings :: [(String, Type)] }

declarationName :: Declaration -> String
declarationName (RecordDeclaration _ name _) = name
declarationName (ValueDeclaration _ name _ _ _) = name

expressionOffset :: Expression -> Int
expressionOffset (IntLiteral offset _) = offset
expressionOffset (StringLiteral offset _) = offset
expressionOffset (Name offset _) = offset
expressionOffset (Apply offset _ _) = offset

parameterName (Parameter _ n) = n
