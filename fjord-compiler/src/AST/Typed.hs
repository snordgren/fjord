-- A typed AST where all types have been checked and asserted valid. No position 
-- information is retained and all names are resolved. This AST is intended to 
-- be used for optimizations and other program transforms, after the program
-- has been verified correct. 
module AST.Typed where

data Module = 
  Module { 
    moduleName :: String, 
    moduleDeclarations :: [Declaration] 
  }
  
data Expression 
  = Addition Expression Expression 
  | Apply Expression Expression
  | Case Expression [Pattern]
  | IntLiteral Integer 
  | Lambda String Type Expression
  | Name String Type 
  | RecordUpdate Expression [FieldUpdate]
  | StringLiteral String 
  deriving (Eq, Show)

data FieldUpdate = FieldUpdate 
  { 
    fieldUpdateName :: String, 
    fieldUpdateExpression :: Expression 
  }
  deriving (Eq, Show)

data Pattern = Pattern 
  {
    patternConstructor :: String,
    patternVariables :: [(String, Type)],
    patternReturnExpression :: Expression
  }
  deriving (Eq, Show)

data Declaration 
  = EnumDeclaration String [EnumConstructor]
  | RecordDeclaration String [RecordField]
  | ValueDeclaration String [Parameter] Type Expression
  deriving (Eq, Show)

data EnumConstructor = EnumConstructor 
  { 
    enumConstructorName :: String, 
    enumConstructorType :: Type
  }
  deriving (Eq, Show)

data RecordField = 
  RecordField { 
    recordFieldName :: String, 
    recordFieldType :: Type 
  } 
  deriving (Eq, Show)

data Parameter = Parameter { parameterName :: String, parameterType :: Type }
  deriving (Eq, Show)

data Type = TypeName String | BuiltInInt | BuiltInString | FunctionType Type Type
  deriving Eq

instance Show Type where
  show (TypeName s) = s
  show BuiltInInt = "BuiltIn.Int"
  show BuiltInString = "BuiltIn.String"
  show (FunctionType p r) = (show p) ++ " -> " ++ (show r)

  
expressionType :: Expression -> Type
expressionType a = 
  case a of 
    Addition b _ -> expressionType b
    Apply b _ -> returnType $ expressionType b
    Case a p -> expressionType $ patternReturnExpression $ head p
    IntLiteral _ -> BuiltInInt
    Lambda _ t r -> FunctionType t $ expressionType r
    Name _ t -> t
    RecordUpdate a _ -> expressionType a
    StringLiteral _ -> BuiltInString


returnType :: Type -> Type 
returnType (FunctionType _ a) = a
returnType a = a
