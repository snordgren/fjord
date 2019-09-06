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
createDefScope :: Scope U.Type -> [U.Parameter] -> U.Type -> [U.Type] -> Scope U.Type
createDefScope modScope parameters typ implicits = 
  let
    parameterBindings = 
      fmap (\(a, typ) -> (a, typ, Common.InFunction, implicits)) $ 
        List.zip (fmap U.parameterName parameters) (fnParListWithUniq typ implicits)

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
Generates the individual contribution of the definition to the module scope.
-}
scopeContrib :: Common.Origin -> U.Declaration -> Scope U.Type
scopeContrib origin d =
  case d of 
    U.DeclEnumDecl (U.EnumDecl offset name constructors typeVars) -> 
      let 
        genCtorBinding 
          :: U.EnumConstructor 
          -> ScopeValue U.Type
        genCtorBinding c = 
          let 
            retT = 
              U.enumConstructorRetType c

            typeVarList =
              List.intersect typeVars $ U.typeNamesIn retT

            typ = 
              List.foldr (\par ret -> U.FunctionType offset Common.NonUnique par ret) 
                retT $ U.enumConstructorParTypes c

            typeLambdas = 
              List.foldr (\par ret -> U.TypeLambda offset par ret) typ typeVarList
          
            uniq = 
              if (List.length $ U.enumConstructorParTypes c) == 0 then
                Common.Unique
              else
                Common.NonUnique
          in
            (U.enumConstructorName c, typeLambdas, origin, [])

        values = 
          fmap genCtorBinding constructors

        types =
          [(name, origin, Common.TypeRef)]
      in
        Scope values types [] []

    U.DeclImplicitDecl (U.ValDecl offset name t implicits) -> 
      Scope [(name, t, origin, [])] [] [(name, t, origin)] []

    U.DeclRecDecl (U.RecDecl offset name fields typeVars) -> 
      let 
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        ctorType = 
          List.foldr (U.FunctionType offset Common.NonUnique) ctorRetType fieldTypes

        ctorWithTypeVars = 
          List.foldr (\par ret -> U.TypeLambda offset par ret) ctorType typeVars

        ctorRetType = 
          List.foldr (\par f -> U.TypeApply offset f (U.TypeName offset par Common.Unique))
            (U.TypeName offset name Common.Unique) typeVars

        scopeFields :: [(String, U.Type, U.Type, Common.Origin)]
        scopeFields = 
          fmap (\r -> (U.recFieldName r, ctorRetType, U.recFieldType r, origin)) fields

        types = 
          [(name, origin, Common.TypeRef)]

        values :: [ScopeValue U.Type]
        values = 
          [(name, ctorWithTypeVars, origin, [])]
      in 
        Scope values types [] scopeFields
    
    U.DeclValDecl (U.ValDecl _ name t implicits) -> 
      let 
        values = 
          [(filter (\a -> a /= '(' && a /= ')') name, t, origin, implicits)]
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
  -> Either TypeErrorAt (U.Type, Common.Origin, [U.Type])
scopeVariableType scope offset name = 
  Combinators.maybeToRight (offset, "undefined in scope")
    (fmap 
      (\(_, t, origin, implicits) -> (t, origin, implicits)) 
      (List.find (\(n, _, _, _) -> n == name) (scopeValues scope)))


mkScopeFromValues :: [ScopeValue U.Type] -> Scope U.Type 
mkScopeFromValues values =
  Scope values [] [] []
