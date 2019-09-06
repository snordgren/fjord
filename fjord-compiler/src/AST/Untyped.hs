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
  = FunctionType Int Common.Uniqueness Type Type 
  | TupleType Int [Type] Common.Uniqueness
  | TypeApply Int Type Type
  | TypeLambda Int String Type
  | TypeName Int String Common.Uniqueness
  deriving (Eq)


typeUniq :: Type -> Common.Uniqueness
typeUniq t = 
  case t of 
    FunctionType _ uniq _ _ -> uniq
    TupleType _ _ uniq -> uniq
    TypeApply _ f _ -> typeUniq f
    TypeLambda _ _ ret -> typeUniq ret
    TypeName _ _ uniq -> uniq


typeWithUniq :: Common.Uniqueness -> Type -> Type
typeWithUniq uniq t = 
  case t of 
    FunctionType offset _ par ret -> FunctionType offset uniq par ret
    TupleType offset types _ -> TupleType offset (fmap (typeWithUniq uniq) types) uniq
    TypeApply offset f par -> TypeApply offset (typeWithUniq uniq f) par
    TypeLambda offset var typ -> TypeLambda offset var (typeWithUniq uniq typ)
    TypeName offset name _ -> TypeName offset name uniq


instance Show Type where
  show a = 
    case a of 
      FunctionType _ p r _ -> 
        show p ++ " -> " ++ show r

      TupleType _ t _ -> 
        "(" ++ (List.intercalate "," $ fmap show t) ++ ")"

      TypeApply _ f par ->
        "(" ++ show f ++ " " ++ show par ++ ")"
    
      TypeLambda _ n t -> 
        n ++ " => " ++ (show t)

      TypeName _ t _ -> 
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
    FunctionType _ _ par ret -> typeNamesIn par ++ typeNamesIn ret
    TupleType _ types _ -> concatMap typeNamesIn types
    TypeApply _ f par -> typeNamesIn f ++ typeNamesIn par
    TypeLambda _ arg ret -> arg : typeNamesIn ret
    TypeName _ name _ -> [name]


parameterType :: Type -> Maybe Type
parameterType (FunctionType _ _ p _) = Just p
parameterType _ = Nothing


returnType :: Type -> Maybe Type
returnType (FunctionType _ _ ret _) = Just ret
returnType _ = Nothing
