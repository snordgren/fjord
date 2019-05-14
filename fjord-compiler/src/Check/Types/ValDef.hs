module Check.Types.ValDef (
  typeCheckValDef
) where

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
    
    toTypedParam (p, t) =
      do
        typedT <- toTypedType defScope t
        return $ T.Parameter (U.parameterName p) typedT

    implicitParNames :: [String]
    implicitParNames = 
      fmap U.parameterName $ take (length implicits) params


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

    findImplicitDef :: U.Type -> Maybe (String, U.Type, Common.Origin)
    findImplicitDef t = 
      List.find (\(_, it, _) -> compareTypEq t it) $ U.scopeImplicits defScope 

    resolveImplicit :: (String, U.Type) -> Either TypeError (String, T.Type, T.Expression)
    resolveImplicit (a, t) = 
      do
        (name, _, orig) <- Combinators.maybeToRight (ImplicitNotFound offset t a) $ findImplicitDef t
        typedT <- toTypedType defScope t
        return (a, typedT, T.Name name typedT Common.NonUnique orig)

  in do
    reqTypeT <- toTypedType defScope reqType
    declTypeT <- toTypedType defScope declType
    paramsT <- Monad.sequence $ fmap toTypedParam $ zip (drop (length implicits) params) $ fnParamList declType
    implicitsT <- Monad.sequence $ fmap resolveImplicit $ zip implicitParNames implicits
    inferredType <- inferType defScope (Just reqTypeT) expr
    typedExpr <- toTypedExpression defScope (Just reqType) expr 
    if inferredType == reqTypeT then 
      Right $ T.ValDef name paramsT implicitsT declTypeT typedExpr
    else
      Left $ WrongType (U.expressionOffset expr) reqTypeT inferredType



      