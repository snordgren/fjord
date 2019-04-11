module Canonicalize (canonicalize, CanonicalizationError (..)) where

import qualified Data.Either.Combinators as Combinators
import qualified Control.Monad as Monad
import qualified Data.List as List

import AST.Canonical as C
import AST.Contextual as D

data CanonicalizationError = TypeNotFound Int String 

canonicalize :: D.Module -> Either CanonicalizationError C.Module 
canonicalize m = 
  let 
    scope = buildScope m
  in do
    declarations <- Monad.sequence (fmap (canonicalizeDeclaration scope) (D.moduleDeclarations m))
    return $ C.Module (D.moduleName m) declarations 

    
buildScope :: D.Module -> D.Scope
buildScope m = 
  let 
    checkDeclaration :: D.Declaration -> [(String, String)]
    checkDeclaration (D.EnumDeclaration offset name fields) = [(name, name)]
    checkDeclaration (D.RecordDeclaration offset name fields) = [(name, name)]
    checkDeclaration (D.ValueDeclaration _ _ _ _ _) = []
  in D.Scope $ List.concat (fmap checkDeclaration (D.moduleDeclarations m))


canonicalizeDeclaration 
  :: D.Scope
  -> D.Declaration 
  -> Either CanonicalizationError C.Declaration

canonicalizeDeclaration scope (D.EnumDeclaration offset name constructors) = 
  let
    resolveConstructor (D.EnumConstructor offset name t) = 
      fmap (C.EnumConstructor offset name) (resolveType scope t) 
  in do
    resolvedConstructors <- Monad.sequence (fmap resolveConstructor constructors)
    return $ C.EnumDeclaration offset name resolvedConstructors

canonicalizeDeclaration scope (D.RecordDeclaration offset name fields) = 
  let 
    canonicalizeField (D.RecordField offset name t) = 
      fmap (C.RecordField offset name) (resolveType scope t)
  in do
    canonicalizedFields <- Monad.sequence (fmap canonicalizeField fields)
    return $ C.RecordDeclaration offset name canonicalizedFields

canonicalizeDeclaration scope (D.ValueDeclaration offset name parameters declaredType expr) = 
  do 
    resolvedType <- resolveType scope declaredType
    resolvedExpression <- canonicalizeExpression expr
    return $ C.ValueDeclaration offset name (fmap canonicalizeParameter parameters) 
      resolvedType resolvedExpression


canonicalizeParameter :: D.Parameter -> C.Parameter
canonicalizeParameter (D.Parameter offset name) = C.Parameter offset name


resolveType :: D.Scope -> D.Type -> Either CanonicalizationError C.Type
resolveType _ (D.Named offset "Int") = 
  Right $ C.BuiltInInt offset

resolveType _ (D.Named offset "String") = 
  Right $ C.BuiltInString offset

resolveType scope (D.Named offset name) = 
  let
    resolution :: Maybe (String, String)
    resolution = List.find (\(a, _) -> a == name) (D.scopeTypes scope)

    canonical :: Maybe C.Type
    canonical = fmap (\(_, b) -> C.Canonical offset b) resolution

    err :: CanonicalizationError
    err = TypeNotFound offset name
  in
    Combinators.maybeToRight err canonical

resolveType scope (D.FunctionType offset parameterType returnType) = do
  resolvedParameterType <- resolveType scope parameterType
  resolvedReturnType <- resolveType scope returnType
  return $ C.FunctionType offset resolvedParameterType resolvedReturnType


canonicalizeExpression :: D.Expression -> Either CanonicalizationError C.Expression
canonicalizeExpression (D.IntLiteral offset value) = Right $ C.IntLiteral offset value
canonicalizeExpression (D.StringLiteral offset value) = Right $ C.StringLiteral offset value
canonicalizeExpression (D.Name offset value) = Right $ C.Name offset value
canonicalizeExpression (D.Addition offset a b) = do
  canonA <- canonicalizeExpression a
  canonB <- canonicalizeExpression b
  return $ C.Addition offset canonA canonB
canonicalizeExpression (D.Apply offset a b) = do
  canonA <- canonicalizeExpression a
  canonB <- canonicalizeExpression b
  return $ C.Apply offset canonA canonB
