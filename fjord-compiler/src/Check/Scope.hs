module Check.Scope where

import qualified Data.List as List
import qualified Data.Either.Combinators as Combinators

import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Untyped as U

{-|
Derive the scope of a definition with parameters. 
-}
createDefScope :: U.Scope -> [U.Parameter] -> U.Type -> U.Scope
createDefScope modScope parameters typ = 
  let
    parameterBindings = 
      fmap (\(a, (typ, uniq)) -> (a, typ, uniq, Common.SameModule)) $ 
        List.zip (fmap U.parameterName parameters) (fnParListWithUniq typ)

    defScope = 
      U.Scope parameterBindings [] []
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
          scope = 
            List.foldl' mergeScope emptyScope $ 
              fmap (scopeContrib $ Common.OtherModule $ U.importModule imp) $ 
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
    U.DeclEnumDecl (U.EnumDecl offset name constructors) -> 
      let 
        genCtorBinding :: U.EnumConstructor -> (String, U.Type, Common.Uniqueness, Common.Origin)
        genCtorBinding c = 
          let 
            typ = 
              List.foldr (\a -> \b -> U.LinearFunctionType 0 a b) (U.enumConstructorRetType c) $ U.enumConstructorParTypes c
          
            uniq = 
              if (List.length $ U.enumConstructorParTypes c) == 0 then
                Common.UniqueTopLevel
              else
                Common.NonUnique
          in
            (U.enumConstructorName c, typ, uniq, origin)

        values = 
          fmap genCtorBinding constructors

        types =
          [(name, origin)]
      in
        U.Scope values types []

    U.DeclImplicitDecl (U.ValDecl offset name implicits t) -> 
      U.Scope [(name, t, Common.NonUnique, origin)] [] [(name, t, origin)]

    U.DeclRecDecl (U.RecDecl offset name fields) -> 
      let 
        constructorRetType = 
          U.TypeName offset name
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        constructorType = 
          List.foldr (U.LinearFunctionType offset) constructorRetType fieldTypes

        values = 
          [(name, constructorType, Common.NonUnique, origin)]

        types = 
          [(name, origin)]
      in 
        U.Scope values types []
    
    U.DeclValDecl (U.ValDecl _ name implicits t) -> 
      let 
        values = [(filter (\a -> a /= '(' && a /= ')') name, t, Common.NonUnique, origin)]
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
    (fmap (\(_, t, uniq, orig) -> (t, uniq, orig)) (List.find (\(n, _, _, _) -> n == name) (U.scopeValues scope)))

