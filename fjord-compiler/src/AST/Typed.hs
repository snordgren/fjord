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


expressionType :: Scope -> Expression -> Type
expressionType scope a = 
  case a of 
    Apply f par -> 
      case parType $ concreteType $ expressionType scope f of 
        TypeName pos name -> 
          returnType $ replaceTypeName scope name (expressionType scope par) (expressionType scope f)

        _ ->
          returnType $ expressionType scope f

    Case a p -> expressionType scope $ patRetExpr $ head p
    IntLiteral _ -> TypeName 0 "Int"
    Let _ _ ret -> expressionType scope ret
    Name _ t _ -> t
    RecAccess _ t _ -> t
    RecUpdate a _ -> expressionType scope a
    StringLiteral _ -> TypeName 0 "String"
    Tuple values -> TupleType 0 $ fmap (expressionType scope) values


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
          
      TypeName pos typeName -> 
        if name == typeName && isTypeVar scope typeName then
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

      TypeName _ exprName ->
        if isTypeVar scope exprName then
          [(exprName, t)]
        else
          []

      _ -> []

-- Unify types when the pattern is a function.
unifyTypes :: Scope -> Type -> Type -> Type
unifyTypes scope pat impl =
  let 
    patSubst =
      findPatSubst scope (typeVarsIn scope pat) (parType $ concreteType pat) impl
  in
    List.foldl' (\acc (name, subst) -> replaceTypeName scope name subst acc) pat patSubst

-- Get all the type variables referenced by a type. 
typeVarsIn :: Scope -> Type -> [String]
typeVarsIn scope t =
  case t of 
    FunctionType pos par ret -> typeVarsIn scope par ++ typeVarsIn scope ret
    TupleType pos types -> concatMap (typeVarsIn scope) types
    TypeApply pos f par -> typeVarsIn scope f ++ typeVarsIn scope par
    TypeLambda pos arg ret -> arg : typeVarsIn (addTypeVarToScope arg scope) ret
    TypeName pos name -> 
      let 
        cond :: (String, Common.Origin, Common.NameType) -> Bool
        cond (n, orig, nameType) = n == name && nameType == Common.TypeVar
        result = List.find cond $ scopeTypes scope
      in Maybe.maybeToList $ fmap (\(n, _, _) -> n) result


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
            
        TypeName pos typeName -> 
          if from == typeName then
            TypeName pos to
          else
            TypeName pos typeName
