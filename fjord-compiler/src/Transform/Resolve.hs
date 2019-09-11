{-# LANGUAGE Strict #-}
module Transform.Resolve where

import Debug.Trace
import qualified Data.List as List
import qualified Data.Maybe as Maybe

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


resolveDef :: Scope T.Type -> T.Definition -> Either String T.Definition
resolveDef moduleScope d = 
  case d of
    T.ValDef name parameters typ expr ->
      let
        scope :: Scope T.Type
        scope = mergeScope (definitionScope d) moduleScope
      in do
        resolvedExpr <- resolveExpr scope expr
        return $ T.ValDef name parameters typ resolvedExpr

    _ -> 
      Right $ d


resolveExpr :: Scope T.Type -> T.Expression -> Either String T.Expression
resolveExpr scope expr = 
  case expr of 
    T.Name name typ orig ->
      let 
        implicits = 
          implicitsOf scope name
      in
        if List.length implicits == 0 then 
          return $ T.Name name typ orig
        else 
          resolveImplicitsIn name typ orig 
    _ -> 
      return expr


resolveImplicitsIn name typ orig = 
  return $ T.Name name typ orig


implicitsOf :: Scope T.Type -> String -> [T.Type]
implicitsOf scope name =
  let 
    implicits :: Maybe (ScopeValue T.Type)
    implicits = 
      List.find (\(n, _, _, _) -> n == name) $ scopeValues scope
  in
    concat $ Maybe.maybeToList $ fmap (\(_, _, _, implicits) -> implicits) implicits


moduleScope :: T.Module -> Scope T.Type
moduleScope mod = 
  Scope [] [] [] []


definitionScope :: T.Definition -> Scope T.Type
definitionScope def = 
  Scope [] [] [] []
