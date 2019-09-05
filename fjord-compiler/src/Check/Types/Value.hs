module Check.Types.Value (
  typeCheckValDecl
) where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import AST.Scope
import Check.Scope
import Check.Types.Common
import Check.Types.Expression
import Check.Types.Infer
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U



bodyUniq :: Common.Uniqueness
bodyUniq = 
  Common.Unique


typeCheckValDecl 
  :: [U.Parameter] 
  -> U.Expression
  -> (String -> [T.Parameter] -> T.Type -> T.Expression -> T.Definition)
  -> Scope U.Type 
  -> U.ValDecl 
  -> Either TypeErrorAt T.Definition
typeCheckValDecl params expr f modScope (U.ValDecl offset name declType implicits) =
  let 
    defScope :: Scope U.Type
    defScope = 
      createDefScope modScope params declType

    reqType :: U.Type
    reqType = 
      inferRequiredBody implicits declType params
    
    toTypedParam (p, (t, uniq)) =
      do
        typedT <- toTypedType offset defScope uniq t
        return $ T.Parameter (U.parameterName p) typedT
  
    uniq :: Common.Uniqueness
    uniq = 
      Common.NonUnique
  in do
    reqTypeT <- toTypedType offset defScope bodyUniq reqType
    declTypeT <- toTypedType offset defScope uniq declType
    let parListWithUniq = (fmap (\a -> (a, Common.NonUnique)) implicits) ++ fnParListWithUniq declType
    paramsT <- traverse toTypedParam $ zip params parListWithUniq
    typedExpr <- (runUseCounting (U.expressionOffset expr) defScope) $ toTypedExpression defScope (Just reqType) (Just bodyUniq) expr 
    let exprT = unifyTypes (T.expressionType $ typedExpr) reqTypeT
    if exprT == reqTypeT then 
      Right $ f name paramsT declTypeT typedExpr
    else
      Left (U.expressionOffset expr,  "expression has type " ++ (show exprT) ++ ", expected " ++ (show reqTypeT))


resolveImplicit :: Int -> Scope U.Type -> (String, U.Type) -> Either TypeErrorAt (String, T.Type, T.Expression)
resolveImplicit offset defScope (a, t) = 
  let 
    uniq = Common.NonUnique
  in 
    do
      (name, _, orig) <- Combinators.maybeToRight (offset, 
        "cannot find implicit " ++ a ++ " of type " ++ show t) $ findImplicitDef defScope t
      typedT <- toTypedType offset defScope uniq t
      return (a, typedT, T.Name name typedT uniq orig)


findImplicitDef :: Scope U.Type -> U.Type -> Maybe (String, U.Type, Common.Origin)
findImplicitDef defScope t = 
  List.find (\(_, it, _) -> compareTypEq t it) $ scopeImplicits defScope 


compareTypEq :: U.Type -> U.Type -> Bool
compareTypEq a b = 
  case a of 
    U.FunctionType _ c d -> 
      case b of 
        U.FunctionType _ e f -> 
          (compareTypEq c e) && (compareTypEq d f)

        U.LinearFunctionType _ e f ->
          (compareTypEq c e) && (compareTypEq d f)

        _ -> 
          False

    U.TypeName _ c ->
      case b of 
        U.TypeName _ d -> 
          c == d

        _ ->
          False

    U.TupleType _ c ->
      case b of 
        U.TupleType _ d -> 
          c == d

        _ ->
          False
