module Check.Types.ValDef (
  typeCheckValDef
) where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Scope
import Check.Types.Expression
import Check.Types.Infer
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

typeCheckValDef :: U.Scope -> U.Definition -> Either TypeError T.Definition
typeCheckValDef modScope (U.ValDef (U.ValDecl offset name implicits declType) params expr) =
  let 
    defScope :: U.Scope
    defScope = 
      createDefScope modScope params declType

    reqType :: U.Type
    reqType = 
      inferRequiredBody declType implicits params
    
    toTypedParam (p, (t, uniq)) =
      do
        typedT <- toTypedType defScope uniq t
        return $ T.Parameter (U.parameterName p) typedT

    implicitParNames :: [String]
    implicitParNames = 
      fmap U.parameterName $ take (length implicits) params
  
    uniq :: Common.Uniqueness
    uniq = 
      Common.NonUnique

    bodyUniq :: Common.Uniqueness
    bodyUniq = 
      Common.Unique
  in do
    reqTypeT <- toTypedType defScope bodyUniq reqType
    declTypeT <- toTypedType defScope uniq declType
    paramsT <- Monad.sequence $ fmap toTypedParam $ zip (drop (length implicits) params) $ fnParListWithUniq declType
    implicitsT <- Monad.sequence $ fmap (resolveImplicit offset defScope) $ zip implicitParNames implicits
    inferredType <- inferType defScope (Just reqTypeT) bodyUniq expr
    typedExpr <- toTypedExpression defScope (Just reqType) bodyUniq expr 
    --trace ("reqTypeT " ++ (show reqTypeT)) (trace ("declTypeT " ++ (show declTypeT)) $ return ())
    if inferredType == reqTypeT then 
      Right $ T.ValDef name paramsT implicitsT declTypeT typedExpr
    else
      Left $ WrongType (U.expressionOffset expr) reqTypeT inferredType


resolveImplicit :: Int -> U.Scope -> (String, U.Type) -> Either TypeError (String, T.Type, T.Expression)
resolveImplicit offset defScope (a, t) = 
  let 
    uniq = Common.NonUnique
  in 
    do
      (name, _, orig) <- Combinators.maybeToRight (ImplicitNotFound offset t a) $ findImplicitDef defScope t
      typedT <- toTypedType defScope uniq t
      return (a, typedT, T.Name name typedT uniq orig)


findImplicitDef :: U.Scope -> U.Type -> Maybe (String, U.Type, Common.Origin)
findImplicitDef defScope t = 
  List.find (\(_, it, _) -> compareTypEq t it) $ U.scopeImplicits defScope 


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
