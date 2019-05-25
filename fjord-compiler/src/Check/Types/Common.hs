module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

data TypeError 
  = CannotInferType Int String
  | ExpectedUnique Int
  | ImplicitNotFound Int U.Type String
  | ImportNotFound U.Import
  | MixedUniquenessInTuple Int 
  | TooManyParameters Int Int
  | TooManyUsages Int String
  | TooFewUsages Int String
  | UndefinedInScope Int
  | UnknownFieldType Int String T.Type
  | UnknownType Int String
  | WrongType Int T.Type T.Type -- expected type comes first, actual type second
  deriving (Eq, Show)
  

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
