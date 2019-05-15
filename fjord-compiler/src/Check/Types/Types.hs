module Check.Types.Types where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

data TypeError 
  = CannotInferType Int String
  | ImplicitNotFound Int U.Type String
  | ImportNotFound U.Import
  | MixedUniquenessInTuple Int 
  | TooManyParameters Int Int
  | TooManyUsages Int String
  | TooFewUsages Int String
  | UndefinedInScope Int
  | UndefinedType Int String
  | WrongType Int T.Type T.Type 
  deriving (Eq, Show)


parameterType :: U.Type -> Maybe U.Type
parameterType (U.FunctionType _ p _) = Just p
parameterType (U.LinearFunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: U.Type -> Maybe U.Type
returnType (U.FunctionType _ _ ret) = Just ret
returnType (U.LinearFunctionType _ _ ret) = Just ret
returnType _ = Nothing


{-
Get all the parameters of this type.
-}
fnParamList :: U.Type -> [U.Type]
fnParamList t = 
  case t of 
    U.FunctionType _ par ret -> 
      par : fnParamList ret

    U.LinearFunctionType _ par ret ->
      par : fnParamList ret

    _ -> 
      []

{-
Get all the parameters of this type.
-}
fnParListWithUniq :: U.Type -> [(U.Type, Common.Uniqueness)]
fnParListWithUniq t = 
  case t of 
    U.FunctionType _ par ret -> 
      (par, Common.NonUnique) : fnParListWithUniq ret

    U.LinearFunctionType _ par ret ->
      (par, Common.Unique) : fnParListWithUniq ret

    _ -> 
      []


fnTypeList :: U.Type -> [U.Type]
fnTypeList t =
  case t of 
    U.FunctionType _ p r -> 
      p : fnTypeList r
      
    U.LinearFunctionType _ p r ->
      p : fnTypeList r

    a -> 
      [a]
