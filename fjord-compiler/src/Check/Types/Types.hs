module Check.Types.Types where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import AST.Common (Type (..))
import AST.Scope
import Check.Types.Common
import Check.Scope
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


{-
Get all the parameters of this type.
-}
fnParamList :: Type -> [Type]
fnParamList t = 
  case t of 
    FunctionType _ par ret -> 
      par : fnParamList ret

    TypeLambda _ _ ret ->
      fnParamList ret

    _ -> 
      []


fnTypeList :: Type -> [Type]
fnTypeList t =
  case t of 
    FunctionType _ p r -> 
      p : fnTypeList r

    a -> 
      [a]
              
unifyTypes :: Scope -> Type -> Type -> Type
unifyTypes scope pat inst =
  let 
    patSubst = 
      T.findPatSubst scope (T.typeVarsIn scope pat) (T.concreteType pat) inst 
  in
    List.foldl' (\acc (name, subst) -> T.replaceTypeName scope name subst acc) pat patSubst
