module Canonicalize (canonicalize, CanonicalizationError (..)) where

import Control.Monad (sequence)

import AST.Canonical as C
import AST.Contextual as D

data CanonicalizationError = TypeNotFound Int String 

canonicalize :: D.Module -> Either CanonicalizationError C.Module 
canonicalize m = do
  declarations <- sequence (fmap canonicalizeDeclaration (D.moduleDeclarations m))
  return $ C.Module (D.moduleName m) declarations 

canonicalizeDeclaration :: D.Declaration -> Either CanonicalizationError C.Declaration
canonicalizeDeclaration (D.ValueDeclaration offset name parameters declaredType expr) = do
  resolvedType <- resolveType declaredType
  resolvedExpression <- canonicalizeExpression expr
  return $ C.ValueDeclaration offset name (fmap canonicalizeParameter parameters) 
    resolvedType resolvedExpression

canonicalizeParameter :: D.Parameter -> C.Parameter
canonicalizeParameter (D.Parameter offset name) = C.Parameter offset name

resolveType :: D.Type -> Either CanonicalizationError C.Type
resolveType (D.Named offset "Int") = Right $ C.BuiltInInt offset
resolveType (D.Named offset "String") = Right $ C.BuiltInString offset
resolveType (D.Named offset name) = Left $ TypeNotFound offset name
resolveType (D.FunctionType offset parameterType returnType) = do
  resolvedParameterType <- resolveType parameterType
  resolvedReturnType <- resolveType returnType
  return $ C.FunctionType offset resolvedParameterType resolvedReturnType

canonicalizeExpression :: D.Expression -> Either CanonicalizationError C.Expression
canonicalizeExpression (D.IntLiteral offset value) = Right $ C.IntLiteral offset value
canonicalizeExpression (D.StringLiteral offset value) = Right $ C.StringLiteral offset value
canonicalizeExpression (D.Name offset value) = Right $ C.Name offset value
