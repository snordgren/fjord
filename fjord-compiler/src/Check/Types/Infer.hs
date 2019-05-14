module Check.Types.Infer where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Scope
import Check.Types.Expression
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


inferType :: U.Scope -> Maybe T.Type -> U.Expression -> Either TypeError T.Type
inferType scope expectedType expr = 
  case expr of 
    U.Apply offset a b ->
      do
        inferA <- inferType scope Nothing a
        inferB <- inferType scope Nothing b
        case inferA of 
          T.FunctionType param ret -> Right ret
          T.LinearFunctionType par ret -> Right ret
          _ -> Left $ CannotInferType offset "cannot infer function type"

    U.Case offset expr patterns -> 
      inferType scope expectedType (U.patternExpression (head patterns))

    U.IntLiteral offset _ -> 
      Right T.BuiltInInt

    U.Lambda offset name expr ->
      Combinators.maybeToRight (CannotInferType offset "cannot infer lambda type") expectedType

    U.Name offset name ->
      do
        (unT, uniq, orig) <- scopeVariableType scope offset name
        toTypedType scope unT

    U.Operator offset name a b ->
      fmap (T.returnType . T.returnType) $ inferType scope expectedType (U.Name offset name)

    U.RecUpdate _ target _ ->
      inferType scope expectedType target

    U.StringLiteral offset _ -> 
      Right T.BuiltInString

    -- TODO Propagate expected type if expected type is a tuple.
    U.Tuple offset values -> 
      do
        inferredValueTypes <- Monad.sequence $ fmap (inferType scope Nothing) values
        return $ T.TupleType inferredValueTypes


inferRequiredBody :: U.Type -> [U.Type] -> [U.Parameter] -> U.Type
inferRequiredBody declaredType implicits parameters = 
  let 
    remainingParameters = drop (length parameters) (fnParamList declaredType)
    returnType = last (fnTypeList declaredType)
  in if length remainingParameters > 0 then
    List.foldr (U.FunctionType 0) returnType remainingParameters
  else
    returnType
