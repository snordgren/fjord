module AST.Untyped where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }


data Type 
  = TypeName Int String
  | FunctionType Int Type Type 
  | BuiltInInt Int 
  | BuiltInString Int
  deriving (Eq, Show)


data Expression 
  = Apply 
  {
    expressionOffset :: Int,
    applyF :: Expression,
    applyParam :: Expression
  }
  | Case {
    expressionOffset :: Int,
    caseSrcExpr :: Expression,
    casePatterns :: [Pattern]
  }
  | IntLiteral {
    expressionOffset :: Int,
    intLiteralVal :: Integer 
  }
  | Lambda {
    expressionOffset :: Int,
    lambdaVarName :: String,
    lambdaVarExpr :: Expression
  }
  | Name {
    expressionOffset :: Int,
    nameExpressionVal :: String 
  }
  | Operator
  {
    expressionOffset :: Int,
    operatorName :: String,
    operatorLHS :: Expression,
    operatorRHS :: Expression 
  }
  | RecordUpdate {
    expressionOffset :: Int,
    recordUpdateSrcExpr :: Expression,
    recordUpdateFieldUpdates :: [FieldUpdate]
  }
  | StringLiteral {
    expressionOffset :: Int,
    stringLiteralVal :: String 
  }
  deriving (Eq, Show)


data Pattern = 
  Pattern {
    patternOffset :: Int,
    patternConstructor :: String,
    patternValues :: [String],
    patternExpression :: Expression
  }
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
  deriving (Eq, Show)


data EnumConstructor 
  = EnumConstructor {
    enumConstructorOffset :: Int,
    enumConstructorName :: String,
    enumConstructorType :: Type
  }
  deriving (Eq, Show)


data RecordField 
  = RecordField {
    recordFieldOffset :: Int,
    recordFieldName :: String,
    recordFieldType :: Type
  }
  deriving (Eq, Show)


data Parameter 
  = Parameter {
    parameterOffset :: Int,
    parameterName :: String
  }
  deriving (Eq, Show)


data Scope 
  = Scope { 
    scopeBindings :: [(String, Type)]
  }

