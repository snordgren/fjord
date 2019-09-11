{-# LANGUAGE StrictData #-}
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
  deriving (Eq, Show)


data Declaration 
  = DeclEnumDecl EnumDecl
  | DeclImplicitDecl ValDecl
  | DeclRecDecl RecDecl
  | DeclValDecl ValDecl
  deriving (Eq, Show)

data Type 
  = FunctionType Int Type Type 
  | TupleType Int [Type]
  | TypeApply Int Type Type
  | TypeLambda Int String Type
  | TypeName Int String
  deriving (Eq)


instance Show Type where
  show a = 
    case a of 
      FunctionType _ p r -> 
        "(" ++ show p ++ " -> " ++ show r ++ ")"

      TupleType _ t -> 
        "(" ++ (List.intercalate "," $ fmap show t) ++ ")"

      TypeApply _ f par ->
        "(" ++ show f ++ " " ++ show par ++ ")"
    
      TypeLambda _ n t -> 
        n ++ " => " ++ (show t)

      TypeName _ t -> 
        t


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
  | Operator
  {
    expressionOffset :: Int,
    operatorName :: String,
    operatorLHS :: Expression,
    operatorRHS :: Expression 
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
    enumDefCtors :: [EnumConstructor],
    enumDefTypeVars :: [String]
  }
  deriving (Eq, Show)


data RecDecl
  = RecDecl {
    recDeclOffset :: Int,
    recDeclName :: String,
    recDeclFields :: [RecField],
    recDeclTypeVars :: [String]
  }
  deriving (Eq, Show)


data ValDecl
  = ValDecl {
    valDeclOffset :: Int,
    valDeclName :: String,
    valDeclType :: Type,
    valDeclImplicits :: [Type]
  }
  deriving (Eq, Show)


data EnumConstructor 
  = EnumConstructor {
    enumConstructorOffset :: Int,
    enumConstructorName :: String,
    enumConstructorParTypes :: [Type],
    enumConstructorRetType :: Type
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
    TypeName pos name -> [name]


parameterType :: Type -> Maybe Type
parameterType (FunctionType _ p r) = Just p
parameterType _ = Nothing


returnType :: Type -> Maybe Type
returnType (FunctionType _ par ret) = Just ret
returnType _ = Nothing
