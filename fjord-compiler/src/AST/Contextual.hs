module AST.Contextual where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }

data Type = Named Int String |
  FunctionType Int Type Type
  deriving (Eq, Show)

data Expression 
  = IntLiteral Int Integer 
  | StringLiteral Int String 
  | Name Int String 
  | Addition Int Expression Expression 
  | Apply Int Expression Expression
  | Lambda Int String Expression
  | RecordUpdate Int Expression [FieldUpdate]
  deriving (Eq, Show)

data FieldUpdate = FieldUpdate 
  { 
    fieldUpdateOffset :: Int,
    fieldUpdateName :: String,
    fieldUpdateExpression :: Expression
  }
  deriving (Eq, Show)

data Declaration 
  = EnumDeclaration Int String [EnumConstructor]
  | RecordDeclaration Int String [RecordField]
  | ValueDeclaration Int String [Parameter] Type Expression

data EnumConstructor = EnumConstructor Int String Type

data RecordField = RecordField Int String Type

data Parameter = Parameter Int String

data Scope = Scope { scopeTypes :: [(String, String)] }
