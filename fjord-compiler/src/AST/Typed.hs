{-# LANGUAGE StrictData #-}
-- A typed AST where all types have been checked and asserted valid. No position 
-- information is retained and all names are resolved. This AST is intended to 
-- be used for optimizations and other program transforms, after the program
-- has been verified correct. 
module AST.Typed where

import Control.Monad.Writer.Lazy
import Debug.Trace
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified AST.Common as Common


data Module = 
  Module { 
    moduleName :: String, 
    moduleImports :: [Import],
    moduleDefs :: [Definition] 
  }


data Import 
  = Import {
    importModule :: String,
    importPath :: String
  } 
  deriving (Eq, Show)

  
data Expression 
  = Apply Expression Expression
  | Case Expression [Pattern]
  | IntLiteral Integer
  | Let String Expression Expression
  | Name String Type Common.Origin
  | RecAccess String Type Expression
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
    patRetExpr :: Expression
  }
  deriving (Eq, Show)

data Definition 
  = EnumDef String [EnumConstructor]
  | ImplicitDef String Type Expression
  | RecDef String [RecField]
  | ValDef String [Parameter] Type Expression
  deriving (Eq, Show)


data EnumConstructor = EnumConstructor 
  { 
    enumConstructorName :: String, 
    enumConstructorPars :: [Type],
    enumConstructorRet :: Type
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
  = FunctionType Type Type
  | TupleType [Type]
  | TypeApply Type Type
  | TypeLambda String Type
  | TypeName String Common.NameType
  deriving Eq

instance Show Type where
  show a =
    case a of 
      FunctionType p r ->
        "(" ++ show p ++ " -> " ++ show r ++ ")"

      TupleType values -> 
        "(" ++ List.intercalate ", " (fmap show values) ++ ")"

      TypeApply f par ->
        "(" ++ show f ++ " " ++ show par ++ ")"

      TypeLambda arg ret -> 
        arg ++ ". " ++ show ret

      TypeName s nameType -> 
        s


expressionType :: Expression -> Type
expressionType a = 
  case a of 
    Apply f par -> 
      case parType $ concreteType $ expressionType f of 
        TypeName name Common.TypeVar -> 
          returnType $ replaceTypeName name (expressionType par) (expressionType f)

        _ ->
          returnType $ expressionType f

    Case a p -> expressionType $ patRetExpr $ head p
    IntLiteral _ -> TypeName "Int" Common.TypeRef
    Let _ _ ret -> expressionType ret
    Name _ t _ -> t
    RecAccess _ t _ -> t
    RecUpdate a _ -> expressionType a
    StringLiteral _ -> TypeName "String" Common.TypeRef
    Tuple values -> TupleType $ fmap expressionType values


parType :: Type -> Type
parType a = 
  let 
    params = fnParamList a
  in
    if null params then
      error $ show a ++ " has no parameters."
    else
      head params


returnType :: Type -> Type 
returnType t =
  case concreteType t of 
    FunctionType _ a -> 
      a

    TypeLambda _ ret -> 
      returnType ret

    a ->
      error $ show a ++ " has no return type"


concreteType :: Type -> Type 
concreteType t =
  case t of 

    TypeLambda var ret -> 
      concreteType ret

    a -> 
      a


replaceTypeName :: String -> Type -> Type -> Type
replaceTypeName name with target =
  let 
    next = 
      replaceTypeName name with
  in
    case target of 
      FunctionType par ret -> FunctionType (next par) $ next ret
      TupleType types -> TupleType $ fmap next types
      TypeApply f par -> TypeApply (next f) $ next par
      TypeLambda arg ret -> 
        if arg == name then
          next ret
        else
          TypeLambda arg $ next ret
          
      TypeName typeName nameType -> 
        if name == typeName then
          with
        else
          TypeName typeName nameType



findPatSubst :: [String] -> Type -> Type -> [(String, Type)]
findPatSubst typeVars exprType t = 
  let 
    self = 
      findPatSubst typeVars
  in
    case exprType of
      FunctionType exprPar exprRet -> 
        case t of 
          FunctionType tPar tRet ->
            self exprPar tPar ++ self exprRet tRet

          _ -> []

      TypeApply exprF exprPar ->
        case t of 
          TypeApply tF tPar ->
            self exprF tF ++ self exprPar tPar 

          _ -> []

      TypeName exprName exprNameType ->
        if exprNameType == Common.TypeVar then
          [(exprName, t)]
        else
          []

      _ -> []

-- Unify types when the pattern is a function.
unifyTypes :: Type -> Type -> Type
unifyTypes pat impl =
  let 
    patSubst =
      findPatSubst (typeVarsIn pat) (parType $ concreteType pat) impl
  in
    List.foldl' (\acc (name, subst) -> replaceTypeName name subst acc) pat patSubst

-- Get all the type variables referenced by a type. 
typeVarsIn :: Type -> [String]
typeVarsIn t =
  case t of 
    FunctionType par ret -> typeVarsIn par ++ typeVarsIn ret
    TupleType types -> concatMap typeVarsIn types
    TypeApply f par -> typeVarsIn f ++ typeVarsIn par
    TypeLambda arg ret -> arg : typeVarsIn ret
    TypeName name nameType -> 
      case nameType of 
        Common.TypeRef -> []
        Common.TypeVar -> [name]


fnParamList :: Type -> [Type]
fnParamList t = 
  case t of 

    FunctionType a b ->
      a : fnParamList b

    TypeLambda _ ret -> 
      fnParamList ret

    _ -> []


renameTypeVar :: String -> String -> Type -> Type
renameTypeVar from to t =
    let 
      next = 
        renameTypeVar from to
    in
      case t of 
        FunctionType par ret -> FunctionType (next par) $ next ret
        TupleType types -> TupleType $ fmap next types
        TypeApply f par -> TypeApply (next f) $ next par
        TypeLambda arg ret -> 
          if arg == from then
            TypeLambda to $ next ret
          else
            TypeLambda arg $ next ret
            
        TypeName typeName nameType -> 
          if from == typeName then
            TypeName to nameType
          else
            TypeName typeName nameType
