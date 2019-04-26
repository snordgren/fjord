-- A typed AST where all types have been checked and asserted valid. No position 
-- information is retained and all names are resolved. This AST is intended to 
-- be used for optimizations and other program transforms, after the program
-- has been verified correct. 
module AST.Typed where

import qualified Data.List as List


data Module = 
  Module { 
    moduleName :: String, 
    moduleDefs :: [Definition] 
  }
  
data Expression 
  = Apply Expression Expression
  | Case Expression [Pattern]
  | IntLiteral Integer 
  | Lambda String Type Expression
  | Name String Type 
  | Operator String Type Expression Expression 
  | RecUpdate Expression [FieldUpdate]
  | StringLiteral String 
  | Tuple [Expression]
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

data Definition 
  = EnumDef String [EnumConstructor]
  | ImplicitDef String Type Expression
  | RecDef String [RecField]
  | ValDef String [Parameter] [(String, Type, Expression)] Type Expression
  deriving (Eq, Show)


data EnumConstructor = EnumConstructor 
  { 
    enumConstructorName :: String, 
    enumConstructorType :: Type
  }
  deriving (Eq, Show)


data RecField = 
  RecField { 
    recFieldName :: String, 
    recFieldType :: Type 
  } 
  deriving (Eq, Show)


data Parameter 
  = Parameter { 
    parameterName :: String, 
    parameterType :: Type 
  }
  deriving (Eq, Show)

data Type 
  = BuiltInInt 
  | BuiltInString 
  | FunctionType Type Type
  | TupleType [Type]
  | TypeName String 
  deriving Eq

instance Show Type where
  show BuiltInInt = "BuiltIn.Int"
  show BuiltInString = "BuiltIn.String"
  show (FunctionType p r) = (show p) ++ " -> " ++ (show r)
  show (TupleType values) = "(" ++ (List.intercalate "," $ fmap show values) ++ ")"
  show (TypeName s) = s
  
expressionType :: Expression -> Type
expressionType a = 
  case a of 
    Apply b _ -> returnType $ expressionType b
    Case a p -> expressionType $ patternReturnExpression $ head p
    IntLiteral _ -> BuiltInInt
    Lambda _ t r -> FunctionType t $ expressionType r
    Name _ t -> t
    Operator _ t _ _ -> returnType $ returnType t
    RecUpdate a _ -> expressionType a
    StringLiteral _ -> BuiltInString
    Tuple values -> TupleType $ fmap expressionType values


returnType :: Type -> Type 
returnType (FunctionType _ a) = a
returnType a = a
