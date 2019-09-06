module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

type TypeError = String
type TypeErrorAt = (Int, TypeError)

{-
Get all the parameters of this type.
-}
fnParListWithUniq :: U.Type -> [U.Type] -> [U.Type]
fnParListWithUniq t implicits = 
  case t of 
    U.FunctionType _ _ par ret -> 
      implicits ++ (par : fnParListWithUniq ret [])

    U.TypeLambda _ _ ret ->
      implicits ++ (fnParListWithUniq ret [])

    _ -> 
      []
