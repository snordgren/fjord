module Check.Types.Common where

import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

type TypeError = String
type TypeErrorAt = (Int, TypeError)

{-
Get all the parameters of this type.
-}
fnParList :: U.Type -> [U.Type] -> [U.Type]
fnParList t implicits = 
  case t of 
    U.FunctionType _ par ret -> 
      implicits ++ (par : fnParList ret [])

    U.TypeLambda _ _ ret ->
      implicits ++ (fnParList ret [])

    _ -> 
      []
