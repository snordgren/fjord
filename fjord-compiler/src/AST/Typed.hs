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
  | IntLiteral Integer Common.Uniqueness
  | Lambda String Type Expression
  | Let String Expression Expression
  | Name String Type Common.Uniqueness Common.Origin
  | Operator Expression Type Expression Expression Common.Origin
  | RecAccess String Type Expression
  | RecUpdate Expression [FieldUpdate]
  | StringLiteral String Common.Uniqueness
  | Tuple Common.Uniqueness [Expression]
  | UniqueLambda String Type Expression
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
  = FunctionType Common.Uniqueness Type Type
  | LinearFunctionType Type Type
  | TupleType Common.Uniqueness [Type]
  | TypeApply Type Type
  | TypeLambda String Type
  | TypeName Common.Uniqueness String Common.NameType
  deriving Eq

instance Show Type where
  show a =
    case a of 
      FunctionType uniq p r ->
        let
          base = 
            (show p) ++ " -> " ++ (show r)
        in
          case uniq of 
            Common.Unique -> 
              uniqPrefix uniq ++ "(" ++ base ++ ")"
    
            Common.NonUnique ->
              base
              
      LinearFunctionType p r -> 
        (show p) ++ " -* " ++ (show r)

      TupleType uniq values -> 
        (uniqPrefix uniq) ++ "(" ++ (List.intercalate ", " $ fmap show values) ++ ")"

      TypeApply f par ->
        "(" ++ show f ++ " " ++ show par ++ ")"

      TypeLambda arg ret -> 
        arg ++ ". " ++ show ret

      TypeName uniq s nameType -> 
        uniqPrefix uniq ++ s


uniqPrefix :: Common.Uniqueness -> String
uniqPrefix a =
  case a of 
    Common.Unique -> "1:"
    Common.NonUnique -> "?:"


expressionType :: Expression -> Type
expressionType a = 
  case a of 
    Apply f par -> 
      case parType $ concreteType $ expressionType f of 
        TypeName uniq name Common.TypeVar -> 
          returnType $ replaceTypeName name (expressionType par) (expressionType f)

        _ ->
          if (parTypeUniq $ concreteType $ expressionType f) == Common.Unique then
            withUniq Common.Unique $ returnType $ expressionType f
          else
            returnType $ expressionType f

    Case a p -> expressionType $ patRetExpr $ head p
    IntLiteral _ uniq -> TypeName uniq "Int" Common.TypeRef
    Lambda _ t r -> FunctionType Common.NonUnique t $ expressionType r
    Let _ _ ret -> expressionType ret
    Name _ t _ _ -> t
    Operator _ t _ _ _ -> returnType $ returnType t
    RecAccess _ t _ -> t
    RecUpdate a _ -> expressionType a
    StringLiteral _ uniq -> TypeName uniq "String" Common.TypeRef
    Tuple uniq values -> TupleType uniq $ fmap expressionType values
    UniqueLambda _ t r -> LinearFunctionType t $ expressionType r


typeUniq :: Type -> Common.Uniqueness
typeUniq t = 
  case t of 
    FunctionType uniq _ _ -> 
      uniq

    LinearFunctionType _ _ ->
      Common.Unique

    TypeApply f par ->
      typeUniq f

    TypeLambda arg ret ->
      typeUniq ret

    TupleType uniq _ ->
      uniq
    
    TypeName uniq _ _ ->
      uniq


parType :: Type -> Type
parType a = 
  let 
    params = fnParamList a
  in
    if length params <= 0 then
      error $ show a ++ " has no parameters."
    else
      head params


parTypeUniq :: Type -> Common.Uniqueness
parTypeUniq =
  (typeUniq . parType)


returnType :: Type -> Type 
returnType t =
  case concreteType t of 
    FunctionType _ _ a -> 
      a

    LinearFunctionType _ a ->
      a

    TypeLambda _ ret -> 
      returnType ret

    a ->
      error $ (show a) ++ " has no return type"


concreteType :: Type -> Type 
concreteType t =
  case t of 

    TypeLambda var ret -> 
      concreteType ret

    a -> 
      a

withUniq :: Common.Uniqueness -> Type -> Type
withUniq uniq t =
  case t of 
    FunctionType _ par ret -> FunctionType uniq par ret
    TupleType _ types -> TupleType uniq $ fmap (withUniq uniq) types
    TypeLambda arg ret -> TypeLambda arg $ withUniq uniq ret
    TypeName _ name nameType -> TypeName uniq name nameType
    a -> a


replaceTypeName :: String -> Type -> Type -> Type
replaceTypeName name with target =
  let 
    next = 
      replaceTypeName name with
  in
    case target of 
      FunctionType uniq par ret -> FunctionType uniq (next par) $ next ret
      LinearFunctionType par ret -> LinearFunctionType (next par) $ next ret
      TupleType uniq types -> TupleType uniq $ fmap next types
      TypeApply f par -> TypeApply (next f) $ next par
      TypeLambda arg ret -> 
        if arg == name then
          next ret
        else
          TypeLambda arg $ next ret
          
      TypeName uniq typeName nameType -> 
        if name == typeName then
          withUniq uniq with
        else
          TypeName uniq typeName nameType



findPatSubst :: [String] -> Type -> Type -> [(String, Type)]
findPatSubst typeVars exprType t = 
  let 
    self = 
      findPatSubst typeVars
  in
    case exprType of
      FunctionType exprUniq exprPar exprRet -> 
        case t of 
          FunctionType tUniq tPar tRet ->
            self exprPar tPar ++ self exprRet tRet

          _ -> []

      LinearFunctionType exprPar exprRet -> 
        case t of 
          LinearFunctionType tPar tRet -> 
            self exprPar tPar ++ self exprRet tRet

          _ -> []

      TypeApply exprF exprPar ->
        case t of 
          TypeApply tF tPar ->
            self exprF tF ++ self exprPar tPar 

          _ -> []

      TypeName exprUniq exprName exprNameType ->
        if exprNameType == Common.TypeVar then
          [(exprName, t)]
        else
          []

      _ -> []

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
    FunctionType uniq par ret -> typeVarsIn par ++ typeVarsIn ret
    LinearFunctionType par ret -> typeVarsIn par ++ typeVarsIn ret
    TupleType uniq types -> List.concat $ fmap typeVarsIn types
    TypeApply f par -> typeVarsIn f ++ typeVarsIn par
    TypeLambda arg ret -> arg : typeVarsIn ret
    TypeName uniq name nameType -> 
      case nameType of 
        Common.TypeRef -> []
        Common.TypeVar -> [name]


fnParamList :: Type -> [Type]
fnParamList t = 
  case t of 

    FunctionType uniq a b ->
      a : fnParamList b

    LinearFunctionType a b ->
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
        FunctionType uniq par ret -> FunctionType uniq (next par) $ next ret
        LinearFunctionType par ret -> LinearFunctionType (next par) $ next ret
        TupleType uniq types -> TupleType uniq $ fmap next types
        TypeApply f par -> TypeApply (next f) $ next par
        TypeLambda arg ret -> 
          if arg == from then
            TypeLambda to $ next ret
          else
            TypeLambda arg $ next ret
            
        TypeName uniq typeName nameType -> 
          if from == typeName then
            TypeName uniq to nameType
          else
            TypeName uniq typeName nameType


implicitsIn :: Type -> [Type]
implicitsIn t =
  case t of
    FunctionType uniq par ret -> implicitsIn par ++ implicitsIn ret
    LinearFunctionType par ret -> implicitsIn par ++ implicitsIn ret
    TupleType uniq types -> List.concat $ fmap implicitsIn types
    TypeApply f par -> implicitsIn par
    TypeLambda arg ret -> implicitsIn ret
    TypeName uniq typeName nameType -> []
