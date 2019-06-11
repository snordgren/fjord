module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

data TypeError
  = CannotInferType String
  | ExpectedNonUnique
  | ExpectedUnique
  | ImplicitNotFound U.Type String
  | ImportNotFound U.Import
  | MixedUniquenessInTuple
  | TooManyParameters Int
  | TooManyUsages String
  | TooFewUsages String
  | UndefinedInScope
  | UnknownFieldType String T.Type
  | UnknownType String
  | WrongType T.Type T.Type -- expected type comes first, actual type second
  deriving (Eq, Show)
  

type TypeErrorAt = (Int, TypeError)


{-
Get all the parameters of this type.
-}
fnParListWithUniq :: U.Type -> [(U.Type, Common.Uniqueness)]
fnParListWithUniq t = 
  case t of 
    U.BindImplicit _ par ret -> 
      (par, Common.NonUnique) : fnParListWithUniq ret
      
    U.FunctionType _ par ret -> 
      (par, Common.NonUnique) : fnParListWithUniq ret

    U.LinearFunctionType _ par ret ->
      (par, Common.Unique) : fnParListWithUniq ret

    U.TypeLambda _ _ ret ->
      fnParListWithUniq ret

    _ -> 
      []
