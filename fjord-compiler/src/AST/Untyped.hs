module AST.Untyped where

data Module 
  = Module { 
    moduleName :: String, 
    moduleDefs :: [Definition]
  }
  deriving (Eq, Show)

data TypeDef 
  = TypeDef {
    typeDefName :: String,
    typeDefDecls :: [Declaration]
  }
  deriving (Eq, Show)

data Declaration 
  = DeclEnumDecl EnumDecl
  | DeclRecDecl RecDecl
  | DeclValDecl ValDecl
  deriving (Eq, Show)

data Type 
  = TypeName Int String
  | FunctionType Int Type Type 
  | TupleType Int [Type]
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
  | RecUpdate {
    expressionOffset :: Int,
    recordUpdateSrcExpr :: Expression,
    recordUpdateFieldUpdates :: [FieldUpdate]
  }
  | StringLiteral {
    expressionOffset :: Int,
    stringLiteralVal :: String 
  }
  | Tuple {
    expressionOffset :: Int,
    tupleValues :: [Expression]
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


data Definition 
  = EnumDef EnumDecl
  | RecDef RecDecl
  | ValDef ValDecl [Parameter] Expression
  deriving (Eq, Show)


data EnumDecl
  = EnumDecl {
    enumDeclOffset :: Int,
    enumDefName :: String,
    enumDefCtors :: [EnumConstructor]
  }
  deriving (Eq, Show)


data RecDecl
  = RecDecl {
    recDeclOffset :: Int,
    recDeclName :: String,
    recDeclFields :: [RecField]
  }
  deriving (Eq, Show)


data ValDecl
  = ValDecl {
    valDeclOffset :: Int,
    valDeclName :: String,
    valDeclType :: Type
  }
  deriving (Eq, Show)


data EnumConstructor 
  = EnumConstructor {
    enumConstructorOffset :: Int,
    enumConstructorName :: String,
    enumConstructorType :: Type
  }
  deriving (Eq, Show)


data RecField 
  = RecField {
    recFieldOffset :: Int,
    recFieldName :: String,
    recFieldType :: Type
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


defToDecl :: Definition -> Declaration
defToDecl d = 
  case d of 
    EnumDef a -> 
      DeclEnumDecl a

    RecDef a -> 
      DeclRecDecl a

    ValDef a _ _ ->
      DeclValDecl a
