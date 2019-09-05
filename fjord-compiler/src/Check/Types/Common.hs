module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

type TypeError = String
type TypeErrorAt = (Int, TypeError)

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

    U.TypeLambda _ _ ret ->
      fnParListWithUniq ret

    _ -> 
      []
