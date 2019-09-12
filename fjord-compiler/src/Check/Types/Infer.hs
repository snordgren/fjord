{-# LANGUAGE Strict #-}
module Check.Types.Infer where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import AST.Common (Type (..))
import Check.Scope
import Check.Types.Common
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

inferRequiredBody :: [Type] -> Type -> [U.Parameter] -> Type
inferRequiredBody implicits declaredType parameters = 
  let 
    concreteType =
      U.concreteType declaredType

    remainingParameters = 
      drop (length parameters) (fnParamList concreteType)

    returnType = 
      last (fnTypeList concreteType)
  in if length remainingParameters > 0 then
    List.foldr (FunctionType 0) returnType remainingParameters
  else
    returnType
