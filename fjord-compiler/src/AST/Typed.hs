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

import AST.Common (Type (..))
import qualified AST.Common as Common
import AST.Scope


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
  = Apply Expression Expression Type
  | Case Expression [Pattern]
  | IntLiteral Integer
  | Let String Expression Expression
  | Name String Type Common.Origin
  | RecAccess String Type Expression
  | RecUpdate Expression [FieldUpdate]
  | StringLiteral String
  | Tuple [Expression]
  deriving (Show)


data FieldUpdate = FieldUpdate 
  { 
    fieldUpdateName :: String, 
    fieldUpdateExpression :: Expression 
  }
  deriving (Show)

data Pattern = Pattern 
  {
    patternConstructor :: String,
    patternVariables :: [(String, Type)],
    patRetExpr :: Expression
  }
  deriving (Show)

data Definition 
  = EnumDef String [EnumConstructor]
  | ImplicitDef String Type Expression
  | RecDef String [RecField]
  | ValDef String [Parameter] Type Expression
  deriving (Show)


data EnumConstructor = EnumConstructor 
  { 
    enumConstructorName :: String, 
    enumConstructorPars :: [Type],
    enumConstructorRet :: Type
  }
  deriving (Show)


data RecField = 
  RecField { 
    recFieldName :: String, 
    recFieldType :: Type 
  } 
  deriving (Show)


data Parameter 
  = Parameter { 
    parameterName :: String, 
    parameterType :: Type 
  }
  deriving (Show)


expressionType :: Expression -> Type
expressionType a = 
  case a of 
    Apply f par t -> t
    Case a p -> expressionType $ patRetExpr $ head p
    IntLiteral _ -> TypeName 0 "Int" Common.TypeRef
    Let _ _ ret -> expressionType ret
    Name _ t _ -> t
    RecAccess _ t _ -> t
    RecUpdate a _ -> expressionType a
    StringLiteral _ -> TypeName 0 "String" Common.TypeRef
    Tuple values -> TupleType 0 $ fmap expressionType values


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
    FunctionType pos _ a -> 
      a

    TypeLambda pos _ ret -> 
      returnType ret

    a ->
      error $ show a ++ " has no return type"


concreteType :: Type -> Type 
concreteType t =
  case t of 
    TypeLambda pos var ret -> 
      concreteType ret

    a -> 
      a


replaceTypeName :: Scope -> String -> Type -> Type -> Type
replaceTypeName scope name with target =
  let 
    next = 
      replaceTypeName scope name with
  in
    case target of 
      FunctionType pos par ret -> FunctionType pos (next par) $ next ret
      TupleType pos types -> TupleType pos $ fmap next types
      TypeApply pos f par -> TypeApply pos (next f) $ next par
      TypeLambda pos arg ret -> 
        if arg == name then
          next ret
        else
          TypeLambda pos arg $ next ret
          
      TypeName pos typeName nameType -> 
        if name == typeName && nameType == Common.TypeVar then
          with
        else
          target



findPatSubst :: Scope -> [String] -> Type -> Type -> [(String, Type)]
findPatSubst scope typeVars exprType t = 
  let 
    self = 
      findPatSubst scope typeVars
  in
    case exprType of
      FunctionType _ exprPar exprRet -> 
        case t of 
          FunctionType _ tPar tRet ->
            self exprPar tPar ++ self exprRet tRet

          _ -> []

      TypeApply _ exprF exprPar ->
        case t of 
          TypeApply _ tF tPar ->
            self exprF tF ++ self exprPar tPar 

          _ -> []

      TypeName _ exprName nameType ->
        if nameType == Common.TypeVar then
          [(exprName, t)]
        else
          []

      _ -> []

-- Unify types when the pattern is a function.
unifyTypes :: Scope -> Type -> Type -> Type
unifyTypes scope pat impl =
  let 
    patSubst =
      findPatSubst scope (typeVarsIn pat) (parType $ concreteType pat) impl
  in
    List.foldl' (\acc (name, subst) -> replaceTypeName scope name subst acc) pat patSubst

-- Get all the type variables referenced by a type. 
typeVarsIn :: Type -> [String]
typeVarsIn t =
  case t of 
    FunctionType pos par ret -> typeVarsIn par ++ typeVarsIn ret
    TupleType pos types -> concatMap typeVarsIn types
    TypeApply pos f par -> typeVarsIn f ++ typeVarsIn par
    TypeLambda pos arg ret -> arg : typeVarsIn ret
    TypeName pos name nameType -> if nameType == Common.TypeVar then [name] else []


fnParamList :: Type -> [Type]
fnParamList t = 
  case t of 

    FunctionType pos a b ->
      a : fnParamList b

    TypeLambda pos _ ret -> 
      fnParamList ret

    _ -> []


renameTypeVar :: String -> String -> Type -> Type
renameTypeVar from to t =
    let 
      next = 
        renameTypeVar from to
    in
      case t of 
        FunctionType pos par ret -> FunctionType pos (next par) $ next ret
        TupleType pos types -> TupleType pos $ fmap next types
        TypeApply pos f par -> TypeApply pos (next f) $ next par
        TypeLambda pos arg ret -> 
          if arg == from then
            TypeLambda pos to $ next ret
          else
            TypeLambda pos arg $ next ret
            
        TypeName pos typeName nameType -> 
          if from == typeName then
            TypeName pos to nameType
          else
            TypeName pos typeName nameType
