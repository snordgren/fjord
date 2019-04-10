module TypeCheck (typeCheck, TypeError (..)) where

import Control.Monad (sequence)
import Data.Either.Combinators as DEC
import qualified Data.List as L

import qualified AST.Canonical as C
import qualified AST.Typed as T

data TypeError = WrongType Int T.Type T.Type |
  CannotInferType Int |
  TooManyParameters Int
  deriving (Eq, Show)

typeCheck :: C.Module -> Either TypeError T.Module
typeCheck m = do
  declarations <- sequence (fmap (toTypedDeclaration (C.moduleDeclarations m))
    (C.moduleDeclarations m))
  return $ T.Module (C.moduleName m) declarations

toTypedDeclaration :: [C.Declaration] -> C.Declaration -> Either TypeError T.Declaration
toTypedDeclaration decls (C.ValueDeclaration offset name parameters declaredType expr) =
  let 
    typedDeclaredType = toTypedType declaredType
    scope = createDeclarationScope decls parameters declaredType
    requiredType = toTypedType (inferRequiredBody declaredType parameters)
    typedParameters = fmap (\(p, t) -> T.Parameter (C.parameterName p) (toTypedType t))
      (zip parameters (functionParameterList declaredType))
  in do
    inferredType <- inferType scope expr
    if inferredType == requiredType then 
      Right(T.ValueDeclaration name typedParameters typedDeclaredType (toTypedExpression expr))
    else
      Left (WrongType (C.expressionOffset expr) requiredType inferredType)

inferRequiredBody :: C.Type -> [C.Parameter] -> C.Type
inferRequiredBody declaredType parameters = 
  let 
    remainingParameters = drop (length parameters) (functionParameterList declaredType)
  in if length remainingParameters > 0 then
      foldl (C.FunctionType 0) (last remainingParameters) (init remainingParameters) 
    else
      last (functionTypeList declaredType)

createDeclarationScope :: [C.Declaration] -> [C.Parameter] -> C.Type -> C.Scope
createDeclarationScope moduleDeclarations parameters typ = 
  let
    parameterBindings = (fmap (\(n, t) -> C.Binding n t) 
      (L.zip (fmap C.parameterName parameters) (functionParameterList typ)))

    declarationBindings :: [C.Binding]
    declarationBindings = fmap (\d -> C.Binding (C.declarationName d) (C.declarationType d)) 
      moduleDeclarations
  in
    C.Scope (parameterBindings ++ declarationBindings)
  
functionParameterList :: C.Type -> [C.Type]
functionParameterList t = 
  case t of 
    C.FunctionType _ parameterType returnType -> 
      parameterType : functionParameterList (returnType)
    _ -> []

functionTypeList :: C.Type -> [C.Type]
functionTypeList t =
  case t of 
    C.FunctionType _ p r -> p : functionTypeList r
    a -> [a]
    

toTypedExpression :: C.Expression -> T.Expression
toTypedExpression (C.IntLiteral _ value) = T.IntLiteral value
toTypedExpression (C.StringLiteral _ value) = T.StringLiteral value
toTypedExpression (C.Name _ value) = T.Name value
toTypedExpression (C.Addition _ a b) = T.Addition (toTypedExpression a) (toTypedExpression b)
toTypedExpression (C.Apply _ a b) = T.Apply (toTypedExpression a) (toTypedExpression b)

inferType :: C.Scope -> C.Expression -> Either TypeError T.Type
inferType _ (C.IntLiteral offset _) = Right T.BuiltInInt 
inferType _ (C.StringLiteral offset _) = Right T.BuiltInString
inferType scope (C.Name offset name) = DEC.maybeToRight 
  (CannotInferType offset)
  (fmap toTypedType (C.scopeVariableType scope name))
inferType scope (C.Addition offset a b) = do
  inferA <- inferType scope a
  inferB <- inferType scope b
  if inferA == inferB then 
    Right $ inferA 
  else 
    Left $ WrongType (C.expressionOffset b) inferA inferB
inferType scope (C.Apply offset a b) = do
  inferA <- inferType scope a
  inferB <- inferType scope b
  case inferA of 
    T.FunctionType param ret -> Right ret
    _ -> Left $ CannotInferType offset

toTypedType (C.BuiltInInt _) = T.BuiltInInt
toTypedType (C.BuiltInString _) = T.BuiltInString
toTypedType (C.Canonical _ n) = T.TypeName n
toTypedType (C.FunctionType offset parameterType returnType) = 
  T.FunctionType (toTypedType parameterType) (toTypedType returnType)
