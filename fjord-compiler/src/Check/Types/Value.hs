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
      createDefScope modScope params declType implicits

    reqType :: U.Type
    reqType = 
      inferRequiredBody implicits declType params
    
    toTypedParam :: (U.Parameter, U.Type) -> Either TypeErrorAt T.Parameter
    toTypedParam (p, t) =
      do
        typedT <- toTypedType offset defScope t
        return $ T.Parameter (U.parameterName p) typedT
  in do
    reqTypeT <- toTypedType offset defScope reqType
    declTypeT <- toTypedType offset defScope declType
    let parListWithUniq = fnParList declType implicits
    paramsT <- traverse toTypedParam $ zip params parListWithUniq
    typedExpr <- (runUseCounting (U.expressionOffset expr) defScope) $ toTypedExpression defScope (Just reqType) expr 
    let exprT = unifyTypes (T.expressionType $ typedExpr) reqTypeT
    if exprT == reqTypeT then 
      Right $ f name paramsT declTypeT typedExpr
    else
      Left (U.expressionOffset expr,  
        "expression has type " ++ (show exprT) ++ ", expected " ++ (show reqTypeT))


compareTypEq :: U.Type -> U.Type -> Bool
compareTypEq a b = 
  case a of 
    U.FunctionType _ c d -> 
      case b of 
        U.FunctionType _ e f -> 
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
