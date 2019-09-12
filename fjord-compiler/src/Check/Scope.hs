{-# LANGUAGE Strict #-}
module Check.Scope where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Either.Combinators as Combinators

import AST.Common (Type (..))
import AST.Scope
import Check.Types.Common
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U

{-|
Derive the scope of a definition with parameters. 
-}
createDefScope :: Scope -> [U.Parameter] -> Type -> [Type] -> Scope
createDefScope modScope parameters typ implicits = 
  let
    parameterBindings = 
      fmap (\(a, typ) -> (a, typ, Common.InFunction, implicits)) $ 
        List.zip (fmap U.parameterName parameters) (fnParList typ implicits)

    typeLambdaValues t =
      case t of 
        TypeLambda _ arg ret -> 
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


deriveImportScope :: [U.TypeDef] -> U.Import -> Scope
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


emptyScope :: Scope
emptyScope =
  Scope [] [] [] []

{-|
Generates the individual contribution of the definition to the module scope.
-}
scopeContrib :: Common.Origin -> U.Declaration -> Scope
scopeContrib origin d =
  case d of 
    U.DeclEnumDecl (U.EnumDecl offset name constructors typeVars) -> 
      let 
        genCtorBinding 
          :: U.EnumConstructor 
          -> ScopeValue
        genCtorBinding c = 
          let 
            retT = 
              U.enumConstructorRetType c

            typeVarList =
              List.intersect typeVars $ U.typeNamesIn retT

            typ = 
              List.foldr (\par ret -> FunctionType offset par ret) 
                retT $ U.enumConstructorParTypes c

            typeLambdas = 
              List.foldr (\par ret -> TypeLambda offset par ret) typ typeVarList
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

    U.DeclRecDecl (U.RecDecl offset recordName fields typeVars) -> 
      let 
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        ctorType = 
          List.foldr (FunctionType offset) ctorRetType fieldTypes

        ctorWithTypeVars = 
          List.foldr (\par ret -> TypeLambda offset par ret) ctorType typeVars

        ctorRetType = 
          List.foldr (\par f -> TypeApply offset f (TypeName offset par Common.TypeVar))
            (TypeName offset recordName Common.TypeRef) typeVars

        scopeFields :: [(String, Type, Type, Common.Origin)]
        scopeFields = 
          fmap (\r -> (U.recFieldName r, ctorRetType, U.recFieldType r, origin)) fields

        types = 
          [(recordName, origin, Common.TypeRef)]

        values :: [ScopeValue]
        values = 
          [(recordName, ctorWithTypeVars, origin, [])]
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
  :: Scope 
  -> Int 
  -> String 
  -> Either TypeErrorAt (Type, Common.Origin, [Type])
scopeVariableType scope offset name = 
  Combinators.maybeToRight (offset, "\"" ++ name ++ "\" is undefined in scope")
    (fmap 
      (\(_, t, origin, implicits) -> (t, origin, implicits)) 
      (List.find (\(n, _, _, _) -> n == name) (scopeValues scope)))


mkScopeFromValues :: [ScopeValue] -> Scope 
mkScopeFromValues values =
  Scope values [] [] []
