module AST.Contextual where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }

data Type = Named Int String |
  FunctionType Int Type Type
  deriving (Eq, Show)

data Expression = IntLiteral Int Integer |
  StringLiteral Int String | 
  Name Int String |
  Addition Int Expression Expression |
  Apply Int Expression Expression
  deriving (Eq, Show)

data Declaration 
  = RecordDeclaration Int String [RecordField]
  | ValueDeclaration Int String [Parameter] Type Expression

data RecordField = RecordField Int String Type

data Parameter = Parameter Int String

data Scope = Scope { scopeTypes :: [(String, String)] }
