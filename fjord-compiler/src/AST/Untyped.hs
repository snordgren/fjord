{-# LANGUAGE StrictData #-}
module AST.Untyped where

import qualified Data.List as List

import AST.Common (Type (..))
import qualified AST.Common as Common

data Module 
  = Module { 
    moduleName :: String, 
    moduleImports :: [Import],
    moduleDefs :: [Definition]
  }
  deriving (Show)


data Import 
  = Import {
    importOffset :: Int,
    importModule :: String
  } 
  deriving (Eq, Show)


data TypeDef 
  = TypeDef {
    typeDefSource :: String,
    typeDefName :: String,
    typeDefDecls :: [Declaration]
  }
  deriving (Show)


data Declaration 
  = DeclEnumDecl EnumDecl
  | DeclImplicitDecl ValDecl
  | DeclRecDecl RecDecl
  | DeclValDecl ValDecl
  deriving (Show)


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
  | Let {
    expressionOffset :: Int,
    letName :: String,
    letValue :: Expression,
    letRet :: Expression
  }
  | Name {
    expressionOffset :: Int,
    nameExpressionVal :: String
  }
  | RecAccess {
    expressionOffset :: Int,
    recAccessField :: String,
    recAccessTarget :: Expression
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
  deriving (Show)


data Pattern = 
  Pattern {
    patternOffset :: Int,
    patternConstructor :: String,
    patternValues :: [String],
    patternExpression :: Expression
  }
  deriving (Show)


data FieldUpdate = FieldUpdate 
  { 
    fieldUpdateOffset :: Int,
    fieldUpdateName :: String,
    fieldUpdateExpression :: Expression
  }
  deriving (Show)


data Definition 
  = EnumDef EnumDecl
  | ImplicitDef ValDecl Expression
  | RecDef RecDecl
  | ValDef ValDecl [Parameter] Expression
  deriving (Show)


data EnumDecl
  = EnumDecl {
    enumDeclOffset :: Int,
    enumDefName :: String,
    enumDefCtors :: [EnumConstructor],
    enumDefTypeVars :: [String]
  }
  deriving (Show)


data RecDecl
  = RecDecl {
    recDeclOffset :: Int,
    recDeclName :: String,
    recDeclFields :: [RecField],
    recDeclTypeVars :: [String]
  }
  deriving (Show)


data ValDecl
  = ValDecl {
    valDeclOffset :: Int,
    valDeclName :: String,
    valDeclType :: Type,
    valDeclImplicits :: [Type]
  }
  deriving (Show)


data EnumConstructor 
  = EnumConstructor {
    enumConstructorOffset :: Int,
    enumConstructorName :: String,
    enumConstructorParTypes :: [Type],
    enumConstructorRetType :: Type
  }
  deriving (Show)


data RecField 
  = RecField {
    recFieldOffset :: Int,
    recFieldName :: String,
    recFieldType :: Type
  }
  deriving (Show)


data Parameter 
  = Parameter {
    parameterOffset :: Int,
    parameterName :: String
  }
  deriving (Eq, Show)


defToDecl :: Definition -> Declaration
defToDecl d = 
  case d of 
    EnumDef a -> 
      DeclEnumDecl a

    ImplicitDef a _ -> 
      DeclImplicitDecl a

    RecDef a -> 
      DeclRecDecl a

    ValDef a _ _ ->
      DeclValDecl a


concreteType :: Type -> Type
concreteType t =
  case t of 

    TypeLambda _ _ ret -> 
      concreteType ret

    a -> 
      a


-- Get all the type names referenced by this type. 
typeNamesIn :: Type -> [String]
typeNamesIn t =
  case t of 
    FunctionType pos par ret -> typeNamesIn par ++ typeNamesIn ret
    TupleType pos types -> concatMap typeNamesIn types
    TypeApply pos f par -> typeNamesIn f ++ typeNamesIn par
    TypeLambda pos arg ret -> arg : typeNamesIn ret
    TypeName pos name nameType -> [name]


parameterType :: Type -> Maybe Type
parameterType (FunctionType _ p r) = Just p
parameterType _ = Nothing


returnType :: Type -> Maybe Type
returnType (FunctionType _ par ret) = Just ret
returnType _ = Nothing
