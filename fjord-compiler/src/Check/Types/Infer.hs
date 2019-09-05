module Check.Types.Infer where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Scope
import Check.Types.Common
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

inferRequiredBody :: U.Type -> [U.Parameter] -> U.Type
inferRequiredBody declaredType parameters = 
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


resolveTupleUniq :: Int -> [Common.Uniqueness] -> Either TypeErrorAt Common.Uniqueness
resolveTupleUniq offset uniqValues =
  if List.length uniqValues == 0 then
    return Common.Unique
  else if List.length (List.nub uniqValues) == 1 then
    return $ List.head uniqValues
  else 
    Left (offset, "mixed uniqueness in tuple")
