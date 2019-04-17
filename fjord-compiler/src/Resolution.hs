module Resolution (resolve, ResolutionError (..)) where

import qualified Data.Either.Combinators as Combinators
import qualified Control.Monad as Monad
import qualified Data.List as List

import AST.Untyped as U

data ResolutionError = TypeNotFound Int String 

resolve :: U.Module -> Either ResolutionError U.Module 
resolve m = 
  let 
    scope = buildScope m
  in do
    declarations <- Monad.sequence (fmap (resolveDeclaration scope) (U.moduleDeclarations m))
    return $ U.Module (U.moduleName m) declarations 

    
buildScope :: U.Module -> U.Scope
buildScope m = 
  let 
    checkDeclaration :: U.Declaration -> [(String, U.Type)]
    checkDeclaration (U.EnumDeclaration offset name fields) = [(name, U.TypeName offset name)]
    checkDeclaration (U.RecordDeclaration offset name fields) = [(name, U.TypeName offset name)]
    checkDeclaration (U.ValueDeclaration _ _ _ _ _) = []
  in U.Scope $ List.concat (fmap checkDeclaration (U.moduleDeclarations m))


resolveDeclaration 
  :: U.Scope
  -> U.Declaration 
  -> Either ResolutionError U.Declaration

resolveDeclaration scope (U.EnumDeclaration offset name constructors) = 
  let
    resolveConstructor (U.EnumConstructor offset name t) = 
      fmap (U.EnumConstructor offset name) (resolveType scope t) 
  in do
    resolvedConstructors <- Monad.sequence (fmap resolveConstructor constructors)
    return $ U.EnumDeclaration offset name resolvedConstructors

resolveDeclaration scope (U.RecordDeclaration offset name fields) = 
  let 
    resolveField (U.RecordField offset name t) = 
      fmap (U.RecordField offset name) (resolveType scope t)
  in do
    resolvedFields <- Monad.sequence (fmap resolveField fields)
    return $ U.RecordDeclaration offset name resolvedFields

resolveDeclaration scope (U.ValueDeclaration offset name parameters declaredType expr) = 
  do 
    resolvedType <- resolveType scope declaredType
    resolvedExpression <- resolveExpression expr
    return $ U.ValueDeclaration offset name (fmap resolveParameter parameters) 
      resolvedType resolvedExpression


resolveParameter :: U.Parameter -> U.Parameter
resolveParameter (U.Parameter offset name) = U.Parameter offset name


resolveType :: U.Scope -> U.Type -> Either ResolutionError U.Type
resolveType _ (U.TypeName offset "Int") = 
  Right $ U.BuiltInInt offset

resolveType _ (U.TypeName offset "String") = 
  Right $ U.BuiltInString offset

resolveType scope (U.TypeName offset name) = 
  let
    resolution :: Maybe (String, U.Type)
    resolution = List.find (\(a, _) -> a == name) (U.scopeBindings scope)

    resolved :: Maybe U.Type
    resolved = fmap snd resolution

    err :: ResolutionError
    err = TypeNotFound offset name
  in
    Combinators.maybeToRight err resolved

resolveType scope (U.FunctionType offset parameterType returnType) = do
  resolvedParameterType <- resolveType scope parameterType
  resolvedReturnType <- resolveType scope returnType
  return $ U.FunctionType offset resolvedParameterType resolvedReturnType


resolveExpression :: U.Expression -> Either ResolutionError U.Expression
resolveExpression (U.IntLiteral offset value) =
  Right $ U.IntLiteral offset value

resolveExpression (U.StringLiteral offset value) = 
  Right $ U.StringLiteral offset value

resolveExpression (U.Name offset value) = 
  Right $ U.Name offset value

resolveExpression (U.Addition offset a b) = do
  resolvedA <- resolveExpression a
  resolvedB <- resolveExpression b
  return $ U.Addition offset resolvedA resolvedB

resolveExpression (U.Apply offset a b) = do
  resolvedA <- resolveExpression a
  resolvedB <- resolveExpression b
  return $ U.Apply offset resolvedA resolvedB

resolveExpression (U.Case offset expr patterns) =
  let 
    resolvePattern :: U.Pattern -> Either ResolutionError U.Pattern
    resolvePattern (U.Pattern offset constructor variables returnExpression) = do
      resolvedReturnExpr <- resolveExpression returnExpression
      return $ U.Pattern offset constructor variables resolvedReturnExpr
  in do
    resolvedExpression <- resolveExpression expr
    resolvedPatterns <- Monad.sequence $ fmap resolvePattern patterns
    return $ U.Case offset resolvedExpression resolvedPatterns

resolveExpression (U.Lambda offset name expr) = do
  resolvedExpr <- resolveExpression expr
  return $ U.Lambda offset name resolvedExpr

resolveExpression (U.RecordUpdate offset target updates) = do
  resolvedTarget <- resolveExpression target
  resolvedUpdates <- Monad.sequence (fmap resolveFieldUpdate updates)
  return $ U.RecordUpdate offset resolvedTarget resolvedUpdates

resolveFieldUpdate :: U.FieldUpdate -> Either ResolutionError U.FieldUpdate
resolveFieldUpdate a = do
  resolvedExpression <- resolveExpression (U.fieldUpdateExpression a)
  return $ U.FieldUpdate (U.fieldUpdateOffset a) (U.fieldUpdateName a) resolvedExpression
