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
  = BindImplicit Int Type Type
  | FunctionType Int Type Type
  | LinearFunctionType Int Type Type
  | TupleType Int [Type]
  | TypeApply Int Type Type
  | TypeLambda Int String Type
  | TypeName Int String
  deriving (Eq)


instance Show Type where
  show a = 
    case a of 
      FunctionType _ p r -> 
        show p ++ " -> " ++ show r

      LinearFunctionType _ p r -> 
        show p ++ " -* " ++ show r

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
  | UniqueLambda {
    expressionOffset :: Int,
    lambdaVarName :: String,
    lambdaVarExpr :: Expression
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
    valDeclImplicits :: [Type],
    valDeclType :: Type
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


data Scope 
  = Scope { 
    scopeValues :: [(String, Type, Common.Uniqueness, Common.Origin)],
    scopeTypes :: [(String, Common.Origin, Common.NameType)],
    scopeImplicits :: [(String, Type, Common.Origin)]
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
    BindImplicit _ _ ret -> 
      ret

    TypeLambda _ _ ret -> 
      ret

    a -> 
      a


-- Get all the type names referenced by this type. 
typeNamesIn :: Type -> [String]
typeNamesIn t =
  case t of 
    FunctionType _ par ret -> typeNamesIn par ++ typeNamesIn ret
    LinearFunctionType _ par ret -> typeNamesIn par ++ typeNamesIn ret
    TupleType _ types -> List.concat $ fmap typeNamesIn types
    TypeApply _ f par -> typeNamesIn f ++ typeNamesIn par
    TypeLambda _ arg ret -> arg : typeNamesIn ret
    TypeName _ name -> [name]


parameterType :: Type -> Maybe Type
parameterType (FunctionType _ p _) = Just p
parameterType (LinearFunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: Type -> Maybe Type
returnType (FunctionType _ _ ret) = Just ret
returnType (LinearFunctionType _ _ ret) = Just ret
returnType _ = Nothing
