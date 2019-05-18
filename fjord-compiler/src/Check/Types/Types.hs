module Check.Types.Types where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Types.Common
import Check.Scope
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


parameterType :: U.Type -> Maybe U.Type
parameterType (U.FunctionType _ p _) = Just p
parameterType (U.LinearFunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: U.Type -> Maybe U.Type
returnType (U.FunctionType _ _ ret) = Just ret
returnType (U.LinearFunctionType _ _ ret) = Just ret
returnType _ = Nothing


{-
Get all the parameters of this type.
-}
fnParamList :: U.Type -> [U.Type]
fnParamList t = 
  case t of 
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

toTypedType :: U.Scope -> Common.Uniqueness -> U.Type -> Either TypeError T.Type
toTypedType scope uniq a =
  case a of 
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
        parT <- toTypedType scope Common.NonUnique par
        retT <- toTypedType scope expectRetUniq ret
        return $ T.FunctionType uniq parT retT

    U.LinearFunctionType _ par ret ->
      do
        parT <- toTypedType scope Common.Unique par
        retT <- toTypedType scope Common.Unique ret
        return $ T.LinearFunctionType parT retT

    U.TupleType _ types ->
      do
        typesT <- Monad.sequence $ fmap (toTypedType scope uniq) types
        return $ T.TupleType uniq typesT

    U.TypeLambda offset var ret ->
      let
        createLambdaScope = 
          mergeScope (U.Scope [] [(var, Common.SameModule, Common.TypeVar)] []) scope
      in
        do
          retT <- toTypedType createLambdaScope uniq ret
          return $ T.TypeLambda var retT

    U.TypeApply offset a b -> 
      do
        typedA <- toTypedType scope uniq a 
        typedB <- toTypedType scope uniq b
        return $ T.TypeApply typedA typedB
    
    U.TypeName offset "Int" -> 
      return $ T.TypeName uniq "Int" Common.TypeRef

    U.TypeName offset "String" -> 
      return $ T.TypeName uniq "String" Common.TypeRef

    U.TypeName offset name ->
      let 
        typeNames = 
          U.scopeTypes scope 

        result = 
          List.find (\(t, _, nameType) -> t == name) typeNames

        resultE =
          Combinators.maybeToRight (UnknownType offset name) result
      in
        fmap (\(t, _, nameType) -> T.TypeName uniq t nameType) resultE

        
unifyTypes :: T.Type -> T.Type -> T.Type
unifyTypes pat inst = 
  case T.concreteType pat of 
    T.TypeApply patF patPar ->
      case inst of
        T.TypeApply instF instPar -> 
          case patPar of 
            T.TypeName uniq name Common.TypeVar -> 
              T.replaceTypeName name instPar pat
            
            _ -> 
              pat

    _ -> 
      pat
              
