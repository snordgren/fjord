module Check.Types.Infer where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Scope
import Check.Types.Expression
import Check.Types.InferUnique
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


inferType :: U.Scope -> Maybe T.Type -> Common.Uniqueness -> U.Expression -> Either TypeError T.Type
inferType scope expectType expectUniq expr = 
  case expr of 
    U.Apply offset a b ->
      let 
        polymorphicType :: T.Type -> Maybe String
        polymorphicType t =
          case t of 
            T.TypeLambda arg _ -> 
              Just arg

            _ -> 
              Nothing

        substituteType :: String -> T.Type -> T.Type -> T.Type
        substituteType name subst t =
          case t of 
            T.FunctionType par ret ->
              T.FunctionType (substituteType name subst par) $ 
                substituteType name subst ret

            T.LinearFunctionType par ret ->
              T.LinearFunctionType (substituteType name subst par) $ 
                substituteType name subst ret

            T.TypeName n -> 
              if n == name then
                subst
              else
                T.TypeName n

            T.TupleType uniq values ->
              T.TupleType uniq $ fmap (substituteType name subst) values

            a -> 
              a

        concreteReturnType inferA inferB ret = 
          case polymorphicType inferA of
            Just poly -> 
              substituteType poly inferB ret

            Nothing -> 
              ret
      in
      do
        inferA <- inferType scope Nothing expectUniq a
        inferB <- inferType scope Nothing expectUniq b
        case T.concreteType inferA of 
          T.FunctionType param ret -> 
            Right $ concreteReturnType inferA inferB ret

          T.LinearFunctionType par ret -> 
            Right $ concreteReturnType inferA inferB ret

          _ -> 
            Left $ CannotInferType offset "cannot infer function type"

    U.Case offset expr patterns -> 
      inferType scope expectType expectUniq (U.patternExpression (head patterns))

    U.IntLiteral offset _ -> 
      Right $ T.BuiltInInt expectUniq

    U.Lambda offset name expr ->
      Combinators.maybeToRight (CannotInferType offset "cannot infer lambda type") expectType

    U.Name offset name ->
      do
        (unT, uniq, orig) <- scopeVariableType scope offset name
        toTypedType scope uniq unT

    U.Operator offset name a b ->
      fmap (T.returnType . T.returnType) $ inferType scope expectType expectUniq (U.Name offset name)

    U.RecUpdate _ target _ ->
      inferType scope expectType expectUniq target

    U.StringLiteral offset _ -> 
      Right $ T.BuiltInString expectUniq

    -- TODO Propagate expected type if expected type is a tuple.
    U.Tuple offset values -> 
      do
        uniqValues <- Monad.sequence $ fmap (inferExprUniq scope expectUniq) values
        uniq <- resolveTupleUniq offset uniqValues
        inferredValueTypes <- Monad.sequence $ fmap (inferType scope Nothing uniq) values
        return $ T.TupleType uniq inferredValueTypes

    U.UniqueLambda offset name expr ->
      Combinators.maybeToRight (CannotInferType offset "cannot infer unique lambda type") expectType


inferRequiredBody :: U.Type -> [U.Type] -> [U.Parameter] -> U.Type
inferRequiredBody declaredType implicits parameters = 
  let 
    concreteType =
      U.concreteType declaredType

    remainingParameters = 
      drop (length parameters) (fnParamList concreteType)

    returnType = 
      last (fnTypeList concreteType)
  in if length remainingParameters > 0 then
    List.foldr (U.FunctionType 0) returnType remainingParameters
  else
    returnType
