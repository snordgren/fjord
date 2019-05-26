{-# LANGUAGE Strict #-}
module Check.Scope where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Either.Combinators as Combinators

import AST.Scope
import Check.Types.Common
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

{-|
Derive the scope of a definition with parameters. 
-}
createDefScope :: Scope U.Type -> [U.Parameter] -> U.Type -> Scope U.Type
createDefScope modScope parameters typ = 
  let
    parameterBindings = 
      fmap (\(a, (typ, uniq)) -> (a, typ, uniq, Common.InFunction)) $ 
        List.zip (fmap U.parameterName parameters) (fnParListWithUniq typ)

    typeLambdaValues t =
      case t of 
        U.TypeLambda _ arg ret -> 
          arg : (typeLambdaValues ret)

        _ -> 
          []

    typeLambdaTypes :: [(String, Common.Origin, Common.NameType)]
    typeLambdaTypes =
      fmap (\a -> (a, Common.SameModule, Common.TypeVar)) $ typeLambdaValues typ

    defScope = 
      Scope parameterBindings typeLambdaTypes [] []
  in
    mergeScope defScope modScope


deriveImportScope :: [U.TypeDef] -> U.Import -> Scope U.Type
deriveImportScope typeDefs imp =
  let
    matchingTypeDef = 
      findTypeDefForImport typeDefs imp
  in
    case matchingTypeDef of 
      Just t -> 
        let 
          modName =
            U.importModule imp

          scope = 
            List.foldl' mergeScope emptyScope $ 
              fmap (scopeContrib $ Common.OtherModule modName) $ 
              U.typeDefDecls t
        in
          scope

      Nothing -> emptyScope


emptyScope :: Scope U.Type
emptyScope =
  Scope [] [] [] []


{-|
Merge two scopes, the tightest bound (innermost) scope should come first. 
-}
mergeScope :: Scope U.Type -> Scope U.Type -> Scope U.Type
mergeScope a b = 
  let 
    mergedValues = scopeValues a ++ scopeValues b
    mergedTypes = scopeTypes a ++ scopeTypes b
    mergedImplicits = scopeImplicits a ++ scopeImplicits b
    mergedFields = scopeFields a ++ scopeFields b
  in
    Scope mergedValues mergedTypes mergedImplicits mergedFields

{-|
Generates the individual contribution of the definition to the module scope.
-}
scopeContrib :: Common.Origin -> U.Declaration -> Scope U.Type
scopeContrib origin d =
  case d of 
    U.DeclEnumDecl (U.EnumDecl offset name constructors typeVars) -> 
      let 
        genCtorBinding :: U.EnumConstructor -> (String, U.Type, Common.Uniqueness, Common.Origin)
        genCtorBinding c = 
          let 
            retT = 
              U.enumConstructorRetType c

            typeVarList =
              List.intersect typeVars $ U.typeNamesIn retT

            typ = 
              List.foldr (\par ret -> U.LinearFunctionType offset par ret) 
                retT $ U.enumConstructorParTypes c

            typeLambdas = 
              List.foldr (\par ret -> U.TypeLambda offset par ret) typ typeVarList
          
            uniq = 
              if (List.length $ U.enumConstructorParTypes c) == 0 then
                Common.Unique
              else
                Common.NonUnique
          in
            (U.enumConstructorName c, typeLambdas, uniq, origin)

        values = 
          fmap genCtorBinding constructors

        types =
          [(name, origin, Common.TypeRef)]
      in
        Scope values types [] []

    U.DeclImplicitDecl (U.ValDecl offset name t) -> 
      Scope [(name, t, Common.NonUnique, origin)] [] [(name, t, origin)] []

    U.DeclRecDecl (U.RecDecl offset name fields typeVars) -> 
      let 
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        ctorType = 
          List.foldr (U.LinearFunctionType offset) ctorRetType fieldTypes

        ctorWithTypeVars = 
          List.foldr (\par ret -> U.TypeLambda offset par ret) ctorType typeVars

        ctorRetType = 
          List.foldr (\par f -> U.TypeApply offset f $ U.TypeName offset par) 
            (U.TypeName offset name) typeVars

        scopeFields :: [(String, U.Type, U.Type, Common.Origin)]
        scopeFields = 
          fmap (\r -> (U.recFieldName r, ctorRetType, U.recFieldType r, origin)) fields

        types = 
          [(name, origin, Common.TypeRef)]

        values = 
          [(name, ctorWithTypeVars, Common.Unique, origin)]
      in 
        Scope values types [] scopeFields
    
    U.DeclValDecl (U.ValDecl _ name t) -> 
      let 
        values = 
          [(filter (\a -> a /= '(' && a /= ')') name, t, Common.Unique, origin)]
      in
        mkScopeFromValues values


findTypeDefForImport :: [U.TypeDef] -> U.Import -> Maybe U.TypeDef
findTypeDefForImport typeDefs imp = 
  let 
    typeDefMatchImport t = 
      U.typeDefName t == (U.importModule imp)
  in
    List.find typeDefMatchImport typeDefs


scopeVariableType 
  :: Scope U.Type 
  -> Int 
  -> String 
  -> Either TypeError (U.Type, Common.Uniqueness, Common.Origin)
scopeVariableType scope offset name = 
  Combinators.maybeToRight (UndefinedInScope offset)
    (fmap 
      (\(_, t, uniq, origin) -> (t, uniq, origin)) 
      (List.find (\(n, _, _, _) -> n == name) (scopeValues scope)))


mkScopeFromValues :: [(String, U.Type, Common.Uniqueness, Common.Origin)] -> Scope U.Type 
mkScopeFromValues values =
  Scope values [] [] []
