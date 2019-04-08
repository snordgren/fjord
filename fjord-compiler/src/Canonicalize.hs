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
canonicalizeDeclaration (D.ValueDeclaration offset name declaredType expr) = do
  resolvedType <- resolveType declaredType
  resolvedExpression <- canonicalizeExpression expr
  return $ C.ValueDeclaration offset name resolvedType resolvedExpression

resolveType :: D.Type -> Either CanonicalizationError C.Type
resolveType (D.Named offset "Int") = Right $ C.BuiltInInt
resolveType (D.Named offset "String") = Right $ C.BuiltInString
resolveType (D.Named offset name) = Left $ TypeNotFound offset name

canonicalizeExpression :: D.Expression -> Either CanonicalizationError C.Expression
canonicalizeExpression (D.IntLiteral offset value) = Right $ C.IntLiteral offset value
canonicalizeExpression (D.StringLiteral offset value) = Right $ C.StringLiteral offset value
