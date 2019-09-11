{-# LANGUAGE Strict #-}
module Transform.Resolve where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import AST.Common (Type (..))
import AST.Scope
import qualified AST.Typed as T

{- Resolve all implicits in this module. -}
resolveModule :: T.Module -> Either String T.Module
resolveModule mod = 
  let
    modScope = moduleScope mod
  in do
    newDefs <- traverse (resolveDef modScope) $ T.moduleDefs mod
    return $ mod { T.moduleDefs = newDefs }


resolveDef :: Scope -> T.Definition -> Either String T.Definition
resolveDef moduleScope d = 
  case d of
    T.ValDef name parameters typ expr ->
      let
        scope :: Scope
        scope = mergeScope (definitionScope d) moduleScope
      in do
        resolvedExpr <- resolveExpr scope expr
        return $ T.ValDef name parameters typ resolvedExpr

    _ -> 
      return d


resolveExpr :: Scope -> T.Expression -> Either String T.Expression
resolveExpr scope expr = 
  case expr of 
    T.Apply f par t -> 
      do
        resolvedF <- resolveExpr scope f
        resolvedPar <- resolveExpr scope par
        return $ T.Apply resolvedF resolvedPar t

    T.Name name typ orig ->
      let 
        implicits = 
          implicitsOf scope name
      in
        if List.length implicits == 0 then 
          return $ T.Name name typ orig
        else 
          resolveImplicitsIn name typ orig implicits

    T.RecAccess fieldName fieldType recordExpr ->
      do
        resolvedRecordExpr <- resolveExpr scope recordExpr
        return $ T.RecAccess fieldName fieldType resolvedRecordExpr

    _ -> 
      trace (show expr) return expr


resolveImplicitsIn name typ orig implicits = 
  trace (show implicits) $ return $ T.Name name typ orig


implicitsOf :: Scope -> String -> [Type]
implicitsOf scope name =
  let 
    implicits :: Maybe ScopeValue
    implicits = 
      List.find (\(n, _, _, _) -> n == name) $ scopeValues scope

    implicitsOnly :: Maybe [Type]
    implicitsOnly =
      fmap (\(_, _, _, implicits) -> implicits) implicits 
  in
    concat $ Maybe.maybeToList implicitsOnly


moduleScope :: T.Module -> Scope
moduleScope mod = 
  Scope [] [] [] []


definitionScope :: T.Definition -> Scope
definitionScope def = 
  Scope [] [] [] []
