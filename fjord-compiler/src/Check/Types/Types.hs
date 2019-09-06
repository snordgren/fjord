module Check.Types.Types where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import AST.Scope
import Check.Types.Common
import Check.Scope
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


{-
Get all the parameters of this type.
-}
fnParamList :: U.Type -> [U.Type]
fnParamList t = 
  case t of 
    U.FunctionType _ _ par ret -> 
      par : fnParamList ret

    U.TypeLambda _ _ ret ->
      fnParamList ret

    _ -> 
      []


fnTypeList :: U.Type -> [U.Type]
fnTypeList t =
  case t of 
    U.FunctionType _ _ p r -> 
      p : fnTypeList r

    a -> 
      [a]

toTypedType :: Int -> Scope U.Type -> U.Type -> Either TypeErrorAt T.Type
toTypedType offset scope a =
  case a of 

    -- TODO Check if this clashes with expected uniqueness.
    U.FunctionType _ functionUniq par ret ->
      let
        expectRetUniq =
          case ret of 
            U.FunctionType _ _ retT _ -> 
              U.typeUniq retT
            
            _ -> 
              Common.Unique
      in
      do
        parT <- toTypedType offset scope par
        retT <- toTypedType offset scope ret
        return $ T.FunctionType (U.typeUniq a) parT retT

    U.TupleType _ types _ ->
      do
        typesT <- traverse (toTypedType offset scope) types
        return $ T.TupleType (U.typeUniq a) typesT

    U.TypeLambda _ var ret ->
      let
        createLambdaScope = 
          mergeScope (Scope [] [(var, Common.SameModule, Common.TypeVar)] [] []) scope
      in
        do
          retT <- toTypedType offset createLambdaScope ret
          return $ T.TypeLambda var retT

    U.TypeApply _ b c -> 
      do
        typedB <- toTypedType offset scope b
        typedC <- toTypedType offset scope c
        return $ T.TypeApply typedB typedC
    
    U.TypeName _ "Int" _ -> 
      return $ T.TypeName (U.typeUniq a) "Int" Common.TypeRef

    U.TypeName _ "String" _ -> 
      return $ T.TypeName (U.typeUniq a) "String" Common.TypeRef

    U.TypeName _ name _ ->
      let 
        typeNames = 
          scopeTypes scope 

        result = 
          List.find (\(t, _, nameType) -> t == name) typeNames

        resultE =
          Combinators.maybeToRight (offset, "unknown type " ++ name) result
      in
        fmap (\(t, _, nameType) -> T.TypeName (U.typeUniq a) t nameType) resultE

        
unifyTypes :: T.Type -> T.Type -> T.Type
unifyTypes pat inst =
  let 
    patSubst = 
      T.findPatSubst (T.typeVarsIn pat) (T.concreteType pat) inst 
  in
    List.foldl' (\acc (name, subst) -> T.replaceTypeName name subst acc) pat patSubst
