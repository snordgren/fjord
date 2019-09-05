module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

type TypeError = String
type TypeErrorAt = (Int, TypeError)

{-
Get all the parameters of this type.
-}
fnParListWithUniq :: U.Type -> [U.Type] -> [(U.Type, Common.Uniqueness)]
fnParListWithUniq t implicits = 
  let 
    uniqImplicits =
      fmap (\a -> (a, Common.NonUnique)) implicits
  in
    case t of 
      U.FunctionType _ par ret -> 
        uniqImplicits ++ ((par, Common.NonUnique) : fnParListWithUniq ret [])

      U.LinearFunctionType _ par ret ->
        uniqImplicits ++ ((par, Common.Unique) : fnParListWithUniq ret [])

      U.TypeLambda _ _ ret ->
        uniqImplicits ++ (fnParListWithUniq ret [])

      _ -> 
        []
