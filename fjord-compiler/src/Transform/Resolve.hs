{-# LANGUAGE Strict #-}
module Transform.Resolve where

import Debug.Trace
import qualified Data.List as List

import qualified AST.Typed as T

{- Resolve all implicits in this module. -}
resolveModule :: T.Module -> Either String T.Module
resolveModule mod = 
  do
    newDefs <- traverse resolveDef $ T.moduleDefs mod
    return $ mod { T.moduleDefs = newDefs }


resolveDef :: T.Definition -> Either String T.Definition
resolveDef d = 
  case d of
    T.ValDef name parameters typ expr ->
      do
        resolvedExpr <- resolveExpr expr
        return $ T.ValDef name parameters typ resolvedExpr

    _ -> 
      Right $ d


resolveExpr :: T.Expression -> Either String T.Expression
resolveExpr expr = 
  case expr of 
    T.Name name typ uniq orig ->
      let 
        implicits = 
          T.implicitsIn typ
      in
        if List.length implicits == 0 then 
          return $ T.Name name typ uniq orig
        else 
          resolveImplicitsIn name typ uniq orig 

    T.Operator op typ leftArg rightArg orig ->
      do
        resolvedOp <- resolveExpr op
        resolvedLeftArg <- resolveExpr leftArg
        resolvedRightArg <- resolveExpr rightArg
        return $ T.Operator resolvedOp typ leftArg rightArg orig

    _ -> 
      return expr


resolveImplicitsIn name typ uniq orig = 
  return $ T.Name name typ uniq orig
