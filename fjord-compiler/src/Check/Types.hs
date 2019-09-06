module Check.Types (
  module Check.Types.Expression,
  module Check.Types.Infer,
  module Check.Types.Types,
  module Check.Types.Value,
  typeCheck
) where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import AST.Scope
import Check.Scope 
import Check.Types.Common
import Check.Types.Expression
import Check.Types.Infer
import Check.Types.Types
import Check.Types.Value (typeCheckValDecl)
import Utils
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


enumParUniq = 
  Common.Unique
  

enumRetUniq =
  Common.Unique


implUniq =
  Common.Unique


recFieldUniq =
  Common.Unique


typeCheck :: [U.TypeDef] -> U.Module -> Either TypeErrorAt T.Module
typeCheck typeDefs m = 
  let

    importScope = 
      List.foldl' mergeScope emptyScope $ fmap (deriveImportScope typeDefs) (U.moduleImports m)

    localDeclScope = 
      List.foldl' mergeScope emptyScope $ 
        fmap ((scopeContrib Common.SameModule) . U.defToDecl) $ 
          U.moduleDefs m

    modScope = 
      mergeScope localDeclScope importScope
  in
    do
      defs <- traverse (toTypedDef modScope) $ U.moduleDefs m
      imports <- traverse (checkImport typeDefs) $ U.moduleImports m
      return $ T.Module (U.moduleName m) imports defs


checkImport :: [U.TypeDef] -> U.Import -> Either TypeErrorAt T.Import
checkImport typeDefs imp = 
  case findTypeDefForImport typeDefs imp of
    Just a -> Right $ T.Import (U.importModule imp) $ U.typeDefSource a
    Nothing -> Left (U.importOffset imp, "cannot find import " ++ importName imp)


importName :: U.Import -> String
importName (U.Import _ name) = name


{-
Generate a scope with the type variables introduced by a definition.
-}
genTypeVarScope :: [String] -> Scope U.Type
genTypeVarScope typeVars = 
  Scope [] (fmap (\str -> (str, Common.SameModule, Common.TypeVar)) typeVars) [] []

{-
Generate a typed definition from an untyped one, or generate an error if there 
is something wrong. 
-}
toTypedDef :: Scope U.Type -> U.Definition -> Either TypeErrorAt T.Definition
toTypedDef modScope a =
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors typeVars) ->
      let 
        enumScope = 
          mergeScope (genTypeVarScope typeVars) modScope

        toTypedEnumConstructor (U.EnumConstructor ctorPos s parTypes retType) = 
          do
            parTypesT <- traverse (toTypedType ctorPos enumScope) parTypes
            retTypeT <- toTypedType ctorPos enumScope retType
            return $ T.EnumConstructor s parTypesT retTypeT
      in do
        ctors <- traverse toTypedEnumConstructor constructors
        return $ T.EnumDef name ctors

    U.ImplicitDef valDecl expr -> 
      typeCheckValDecl [] expr (\name _ t expr -> T.ImplicitDef name t expr) modScope valDecl


    U.RecDef (U.RecDecl offset name fields typeVars) ->
      let
        recScope = 
          mergeScope 
            (genTypeVarScope typeVars)
            modScope

        toTypedRecField (U.RecField fieldPos fieldName fieldType) =
          do
            typedT <- toTypedType fieldPos recScope fieldType
            return $ T.RecField fieldName typedT
      in 
        do
          fieldsT <- traverse toTypedRecField fields
          return $ T.RecDef name fieldsT

    U.ValDef valDecl params expr -> 
      do
        validated <- validateParamCount $ U.ValDef valDecl params expr
        typeCheckValDecl params expr T.ValDef modScope valDecl


validateParamCount :: U.Definition -> Either TypeErrorAt U.Definition
validateParamCount (U.ValDef valDecl params expr) =
  let 
    paramTypes =
      fnParamList $ U.valDeclType valDecl

    maxParamCount =
      (length paramTypes) + (length $ U.valDeclImplicits valDecl)
  in 
    if length params > maxParamCount then
      let 
        offset =
          U.parameterOffset $ head (drop maxParamCount params)
      in
        Left (offset, "too many parameters, expected " ++ (show maxParamCount))
    else
      Right $ U.ValDef valDecl params expr
    