module Check.Types (
  module Check.Types.Expression,
  module Check.Types.Infer,
  module Check.Types.Types,
  module Check.Types.ValDef,
  typeCheck
) where

import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Check.Scope 
import Check.Types.Expression
import Check.Types.Infer
import Check.Types.Types
import Check.Types.ValDef (typeCheckValDef)
import Utils
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


typeCheck :: [U.TypeDef] -> U.Module -> Either TypeError T.Module
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
      defs <- Monad.sequence $ fmap (toTypedDef modScope) $ U.moduleDefs m
      imports <- Monad.sequence $ fmap (checkImport typeDefs) $ U.moduleImports m
      return $ T.Module (U.moduleName m) imports defs


checkImport :: [U.TypeDef] -> U.Import -> Either TypeError T.Import
checkImport typeDefs imp = 
  case findTypeDefForImport typeDefs imp of
    Just a -> Right $ T.Import (U.importModule imp) $ U.typeDefSource a
    Nothing -> Left $ ImportNotFound imp

{-
Generate a typed definition from an untyped one, or generate an error if there 
is something wrong. 
-}
toTypedDef :: U.Scope -> U.Definition -> Either TypeError T.Definition
toTypedDef modScope a =
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors) ->
      let 
        toTypedEnumConstructor (U.EnumConstructor _ s parTypes retType) = 
          do
            parTypesT <- Monad.sequence $ fmap (toTypedType modScope) parTypes
            retTypeT <- toTypedType modScope retType
            return $ T.EnumConstructor s parTypesT retTypeT
      in do
        ctors <- Monad.sequence $ fmap toTypedEnumConstructor constructors
        return $ T.EnumDef name ctors

    U.ImplicitDef (U.ValDecl offset name implicits declType) expr -> 
      let 
        defScope = createDefScope modScope [] declType
        reqType = inferRequiredBody declType implicits []          
      in do
        declTypeT <- toTypedType defScope declType
        reqTypeT <- toTypedType defScope reqType
        inferredType <- inferType defScope (Just reqTypeT) expr
        typedExpr <- toTypedExpression defScope (Just reqType) expr 
        if inferredType == reqTypeT then 
          Right $ T.ImplicitDef name declTypeT typedExpr
        else
          Left $ WrongType (U.expressionOffset expr) reqTypeT inferredType


    U.RecDef (U.RecDecl offset name fields) ->
      let
        toTypedRecField (U.RecField _ s t) =
          do
            typedT <- toTypedType modScope t
            return $ T.RecField s typedT
      in 
        do
          fieldsT <- Monad.sequence $ fmap toTypedRecField fields
          return $ T.RecDef name fieldsT

    U.ValDef valDecl params expr -> 
      do
        validated <- validateParamCount $ U.ValDef valDecl params expr
        typeCheckValDef modScope validated


validateParamCount :: U.Definition -> Either TypeError U.Definition
validateParamCount (U.ValDef valDecl params expr) =
  let 
    paramTypes =
      fnParamList $ U.valDeclType valDecl

    maxParamCount =
      (length $ U.valDeclImplicits valDecl) + (length paramTypes)
  in 
    if length params > maxParamCount then
      let 
        offset =
          U.parameterOffset $ head (drop maxParamCount params)
      in
        Left $ TooManyParameters offset maxParamCount
    else
      Right $ U.ValDef valDecl params expr
    