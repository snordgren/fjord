module AST.Untyped where

import qualified Data.List as List

import qualified AST.Common as Common

data Module 
  = Module { 
    moduleName :: String, 
    moduleImports :: [Import],
    moduleDefs :: [Definition]
  }
  deriving (Eq, Show)


data TypeDef 
  = TypeDef {
    typeDefName :: String,
    typeDefDecls :: [Declaration]
  }
  deriving (Eq, Show)


data Import 
  = Import {
    importModule :: String,
    importSource :: Maybe String
  } 
  deriving (Eq, Show)


data ImportPattern
  = ImportAll  
  | ImportSome [(String, Maybe String)]

data Declaration 
  = DeclEnumDecl EnumDecl
  | DeclImplicitDecl ValDecl
  | DeclRecDecl RecDecl
  | DeclValDecl ValDecl
  deriving (Eq, Show)

data Type 
  = FunctionType Int Type Type
  | TypeName Int String
  | TupleType Int [Type]
  deriving (Eq)


instance Show Type where
  show a = 
    case a of 
      FunctionType _ p r -> show p ++ " -> " ++ show r
      TypeName _ t -> t
      TupleType _ t -> "(" ++ (List.intercalate "," $ fmap show t) ++ ")"


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
  | ImplicitDef ValDecl Expression
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
    valDeclImplicits :: [Type],
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
    scopeValues :: [(String, Type, Common.Origin)],
    scopeTypes :: [(String, Common.Origin)],
    scopeImplicits :: [(String, Type, Common.Origin)]
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
