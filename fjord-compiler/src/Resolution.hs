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
    decls <- Monad.sequence (fmap (resolveDef scope) (U.moduleDefs m))
    return $ U.Module (U.moduleName m) decls 

    
buildScope :: U.Module -> U.Scope
buildScope m = 
  let 
    checkDeclaration :: U.Declaration -> [(String, U.Type)]
    checkDeclaration a = 
      case a of 
        U.DeclEnumDecl (U.EnumDecl offset name fields) ->
          [(name, U.TypeName offset name)]

        U.DeclRecDecl (U.RecDecl offset name fields) ->
          [(name, U.TypeName offset name)]

        U.DeclValDecl (U.ValDecl _ _ _) ->
          []

  in 
    U.Scope $ List.concat (fmap (checkDeclaration . U.defToDecl) (U.moduleDefs m))


resolveDef 
  :: U.Scope
  -> U.Definition 
  -> Either ResolutionError U.Definition
resolveDef scope a = 
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors) ->
      let
        resolveConstructor (U.EnumConstructor offset name t) = 
          fmap (U.EnumConstructor offset name) (resolveType scope t) 
      in do
        resolvedConstructors <- Monad.sequence (fmap resolveConstructor constructors)
        return $ U.EnumDef $ U.EnumDecl offset name resolvedConstructors

    U.RecDef (U.RecDecl offset name fields) ->
      let 
        resolveField (U.RecField offset name t) = 
          fmap (U.RecField offset name) (resolveType scope t)
      in do
        resolvedFields <- Monad.sequence (fmap resolveField fields)
        return $ U.RecDef $ U.RecDecl offset name resolvedFields

    U.ValDef (U.ValDecl offset name declaredType) params expr ->
      do 
        resType <- resolveType scope declaredType
        resExpr <- resolveExpression expr
        return $ U.ValDef (U.ValDecl offset name resType) 
          params resExpr
        

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

resolveType scope (U.TupleType offset types) = do
  resolvedTypes <- Monad.sequence $ fmap (resolveType scope) types
  return $ U.TupleType offset resolvedTypes



resolveExpression :: U.Expression -> Either ResolutionError U.Expression
resolveExpression (U.IntLiteral offset value) =
  Right $ U.IntLiteral offset value

resolveExpression (U.StringLiteral offset value) = 
  Right $ U.StringLiteral offset value

resolveExpression (U.Name offset value) = 
  Right $ U.Name offset value

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

resolveExpression (U.Operator offset opName a b) = do
  resolvedA <- resolveExpression a
  resolvedB <- resolveExpression b
  return $ U.Operator offset opName resolvedA resolvedB

resolveExpression (U.RecUpdate offset target updates) = do
  resolvedTarget <- resolveExpression target
  resolvedUpdates <- Monad.sequence (fmap resolveFieldUpdate updates)
  return $ U.RecUpdate offset resolvedTarget resolvedUpdates

resolveExpression (U.Tuple offset values) = do
  resolvedValues <- Monad.sequence $ fmap resolveExpression values
  return $ U.Tuple offset resolvedValues


resolveFieldUpdate :: U.FieldUpdate -> Either ResolutionError U.FieldUpdate
resolveFieldUpdate a = do
  resolvedExpression <- resolveExpression (U.fieldUpdateExpression a)
  return $ U.FieldUpdate (U.fieldUpdateOffset a) (U.fieldUpdateName a) resolvedExpression
