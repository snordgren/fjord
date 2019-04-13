module Resolution (resolve, ResolutionError (..)) where

import qualified Data.Either.Combinators as Combinators
import qualified Control.Monad as Monad
import qualified Data.List as List

import AST.Contextual as C
import AST.Resolved as R

data ResolutionError = TypeNotFound Int String 

resolve :: C.Module -> Either ResolutionError R.Module 
resolve m = 
  let 
    scope = buildScope m
  in do
    declarations <- Monad.sequence (fmap (resolveDeclaration scope) (C.moduleDeclarations m))
    return $ R.Module (C.moduleName m) declarations 

    
buildScope :: C.Module -> C.Scope
buildScope m = 
  let 
    checkDeclaration :: C.Declaration -> [(String, String)]
    checkDeclaration (C.EnumDeclaration offset name fields) = [(name, name)]
    checkDeclaration (C.RecordDeclaration offset name fields) = [(name, name)]
    checkDeclaration (C.ValueDeclaration _ _ _ _ _) = []
  in C.Scope $ List.concat (fmap checkDeclaration (C.moduleDeclarations m))


resolveDeclaration 
  :: C.Scope
  -> C.Declaration 
  -> Either ResolutionError R.Declaration

resolveDeclaration scope (C.EnumDeclaration offset name constructors) = 
  let
    resolveConstructor (C.EnumConstructor offset name t) = 
      fmap (R.EnumConstructor offset name) (resolveType scope t) 
  in do
    resolvedConstructors <- Monad.sequence (fmap resolveConstructor constructors)
    return $ R.EnumDeclaration offset name resolvedConstructors

resolveDeclaration scope (C.RecordDeclaration offset name fields) = 
  let 
    resolveField (C.RecordField offset name t) = 
      fmap (R.RecordField offset name) (resolveType scope t)
  in do
    resolvedFields <- Monad.sequence (fmap resolveField fields)
    return $ R.RecordDeclaration offset name resolvedFields

resolveDeclaration scope (C.ValueDeclaration offset name parameters declaredType expr) = 
  do 
    resolvedType <- resolveType scope declaredType
    resolvedExpression <- resolveExpression expr
    return $ R.ValueDeclaration offset name (fmap resolveParameter parameters) 
      resolvedType resolvedExpression


resolveParameter :: C.Parameter -> R.Parameter
resolveParameter (C.Parameter offset name) = R.Parameter offset name


resolveType :: C.Scope -> C.Type -> Either ResolutionError R.Type
resolveType _ (C.Named offset "Int") = 
  Right $ R.BuiltInInt offset

resolveType _ (C.Named offset "String") = 
  Right $ R.BuiltInString offset

resolveType scope (C.Named offset name) = 
  let
    resolution :: Maybe (String, String)
    resolution = List.find (\(a, _) -> a == name) (C.scopeTypes scope)

    resolved :: Maybe R.Type
    resolved = fmap (\(_, b) -> R.Canonical offset b) resolution

    err :: ResolutionError
    err = TypeNotFound offset name
  in
    Combinators.maybeToRight err resolved

resolveType scope (C.FunctionType offset parameterType returnType) = do
  resolvedParameterType <- resolveType scope parameterType
  resolvedReturnType <- resolveType scope returnType
  return $ R.FunctionType offset resolvedParameterType resolvedReturnType


resolveExpression :: C.Expression -> Either ResolutionError R.Expression
resolveExpression (C.IntLiteral offset value) =
  Right $ R.IntLiteral offset value

resolveExpression (C.StringLiteral offset value) = 
  Right $ R.StringLiteral offset value

resolveExpression (C.Name offset value) = 
  Right $ R.Name offset value

resolveExpression (C.Addition offset a b) = do
  resolvedA <- resolveExpression a
  resolvedB <- resolveExpression b
  return $ R.Addition offset resolvedA resolvedB

resolveExpression (C.Apply offset a b) = do
  resolvedA <- resolveExpression a
  resolvedB <- resolveExpression b
  return $ R.Apply offset resolvedA resolvedB

resolveExpression (C.Case offset expr patterns) =
  let 
    resolvePattern :: C.Pattern -> Either ResolutionError R.Pattern
    resolvePattern (C.Pattern offset constructor variables returnExpression) = do
      resolvedReturnExpr <- resolveExpression returnExpression
      return $ R.Pattern offset constructor variables resolvedReturnExpr
  in do
    resolvedExpression <- resolveExpression expr
    resolvedPatterns <- Monad.sequence $ fmap resolvePattern patterns
    return $ R.Case offset resolvedExpression resolvedPatterns

resolveExpression (C.Lambda offset name expr) = do
  resolvedExpr <- resolveExpression expr
  return $ R.Lambda offset name resolvedExpr

resolveExpression (C.RecordUpdate offset target updates) = do
  resolvedTarget <- resolveExpression target
  resolvedUpdates <- Monad.sequence (fmap resolveFieldUpdate updates)
  return $ R.RecordUpdate offset resolvedTarget resolvedUpdates

resolveFieldUpdate :: C.FieldUpdate -> Either ResolutionError R.FieldUpdate
resolveFieldUpdate a = do
  resolvedExpression <- resolveExpression (C.fieldUpdateExpression a)
  return $ R.FieldUpdate (C.fieldUpdateOffset a) (C.fieldUpdateName a) resolvedExpression
