{-# LANGUAGE Strict #-}
module Check.Scope where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Either.Combinators as Combinators

import Check.Types.Common
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

{-|
Derive the scope of a definition with parameters. 
-}
createDefScope :: U.Scope -> [U.Parameter] -> U.Type -> U.Scope
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
      U.Scope parameterBindings typeLambdaTypes []
  in
    mergeScope defScope modScope


deriveImportScope :: [U.TypeDef] -> U.Import -> U.Scope
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


emptyScope :: U.Scope
emptyScope =
  U.Scope [] [] []


{-|
Merge two scopes, the tightest bound (innermost) scope should come first. 
-}
mergeScope :: U.Scope -> U.Scope -> U.Scope
mergeScope a b = 
  let 
    mergedValues = U.scopeValues a ++ U.scopeValues b
    mergedTypes = U.scopeTypes a ++ U.scopeTypes b
    mergedImplicits = U.scopeImplicits a ++ U.scopeImplicits b
  in
    U.Scope mergedValues mergedTypes mergedImplicits

{-|
Generates the individual contribution of the definition to the module scope.
-}
scopeContrib :: Common.Origin -> U.Declaration -> U.Scope
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
        U.Scope values types []

    U.DeclImplicitDecl (U.ValDecl offset name t) -> 
      U.Scope [(name, t, Common.NonUnique, origin)] [] [(name, t, origin)]

    U.DeclRecDecl (U.RecDecl offset name fields typeVars) -> 
      let 
        
        fieldTypes = 
          map U.recFieldType fields
    
        ctorType = 
          List.foldr (U.LinearFunctionType offset) ctorRetType fieldTypes

        ctorWithTypeVars = 
          List.foldr (\par ret -> U.TypeLambda offset par ret) ctorType typeVars

        ctorRetType = 
          List.foldr (\par f -> U.TypeApply offset f $ U.TypeName offset par) 
            (U.TypeName offset name) typeVars

        types = 
          [(name, origin, Common.TypeRef)]

        values = 
          [(name, ctorWithTypeVars, Common.Unique, origin)]
      in 
        U.Scope values types []
    
    U.DeclValDecl (U.ValDecl _ name t) -> 
      let 
        values = 
          [(filter (\a -> a /= '(' && a /= ')') name, t, Common.Unique, origin)]
      in
        U.Scope values [] []


findTypeDefForImport :: [U.TypeDef] -> U.Import -> Maybe U.TypeDef
findTypeDefForImport typeDefs imp = 
  let 
    typeDefMatchImport t = 
      U.typeDefName t == (U.importModule imp)
  in
    List.find typeDefMatchImport typeDefs


scopeVariableType 
  :: U.Scope 
  -> Int 
  -> String 
  -> Either TypeError (U.Type, Common.Uniqueness, Common.Origin)
scopeVariableType scope offset name = 
  Combinators.maybeToRight (UndefinedInScope offset)
    (fmap 
      (\(_, t, uniq, origin) -> (t, uniq, origin)) 
      (List.find (\(n, _, _, _) -> n == name) (U.scopeValues scope)))

