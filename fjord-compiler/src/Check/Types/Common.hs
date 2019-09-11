module Check.Types.Common where

import AST.Common (Type (..))
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

type TypeError = String
type TypeErrorAt = (Int, TypeError)

{-
Get all the parameters of this type.
-}
fnParList :: Type -> [Type] -> [Type]
fnParList t implicits = 
  case t of 
    FunctionType _ par ret -> 
      implicits ++ (par : fnParList ret [])

    TypeLambda _ _ ret ->
      implicits ++ (fnParList ret [])

    _ -> 
      []
