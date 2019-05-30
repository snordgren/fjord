module Check.Types.Types where

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
    U.BindImplicit _ par ret -> 
      par : fnParamList ret

    U.FunctionType _ par ret -> 
      par : fnParamList ret

    U.LinearFunctionType _ par ret ->
      par : fnParamList ret

    U.TypeLambda _ _ ret ->
      fnParamList ret

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

toTypedType :: Int -> Scope U.Type -> Common.Uniqueness -> U.Type -> Either TypeErrorAt T.Type
toTypedType offset scope uniq a =
  case a of 
    U.BindImplicit _ par ret -> 
      do
        parT <- toTypedType offset scope uniq par
        retT <- toTypedType offset scope uniq ret
        return $ T.BindImplicit parT retT

    U.FunctionType _ par ret ->
      let
        expectRetUniq =
          case ret of 
            U.FunctionType _ _ _ -> 
              Common.NonUnique
            
            _ -> 
              Common.Unique
      in
      do
        parT <- toTypedType offset scope Common.NonUnique par
        retT <- toTypedType offset scope expectRetUniq ret
        return $ T.FunctionType uniq parT retT

    U.LinearFunctionType _ par ret ->
      do
        parT <- toTypedType offset scope Common.Unique par
        retT <- toTypedType offset scope Common.Unique ret
        return $ T.LinearFunctionType parT retT

    U.TupleType _ types ->
      do
        typesT <- traverse (toTypedType offset scope uniq) types
        return $ T.TupleType uniq typesT

    U.TypeLambda _ var ret ->
      let
        createLambdaScope = 
          mergeScope (Scope [] [(var, Common.SameModule, Common.TypeVar)] [] []) scope
      in
        do
          retT <- toTypedType offset createLambdaScope uniq ret
          return $ T.TypeLambda var retT

    U.TypeApply _ a b -> 
      do
        typedA <- toTypedType offset scope uniq a 
        typedB <- toTypedType offset scope uniq b
        return $ T.TypeApply typedA typedB
    
    U.TypeName _ "Int" -> 
      return $ T.TypeName uniq "Int" Common.TypeRef

    U.TypeName _ "String" -> 
      return $ T.TypeName uniq "String" Common.TypeRef

    U.TypeName _ name ->
      let 
        typeNames = 
          scopeTypes scope 

        result = 
          List.find (\(t, _, nameType) -> t == name) typeNames

        resultE =
          Combinators.maybeToRight (offset, UnknownType name) result
      in
        fmap (\(t, _, nameType) -> T.TypeName uniq t nameType) resultE

        
unifyTypes :: T.Type -> T.Type -> T.Type
unifyTypes pat inst =
  let 
    patSubst = 
      T.findPatSubst (T.typeVarsIn pat) (T.concreteType pat) inst 
  in
    List.foldl' (\acc (name, subst) -> T.replaceTypeName name subst acc) pat patSubst
