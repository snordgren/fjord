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
  declarations <- sequence (fmap toTypedDeclaration (C.moduleDeclarations m))
  return $ T.Module (C.moduleName m) declarations

toTypedDeclaration :: C.Declaration -> Either TypeError T.Declaration
toTypedDeclaration (C.ValueDeclaration offset name parameters declaredType expr) =
  let 
    typedDeclaredType = toTypedType declaredType
    scope = createDeclarationScope parameters declaredType
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

createDeclarationScope :: [C.Parameter] -> C.Type -> C.Scope
createDeclarationScope parameters typ = 
  C.Scope (fmap (\(n, t) -> C.Binding n t) 
    (L.zip (fmap C.parameterName parameters) (functionParameterList typ)))

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

inferType :: C.Scope -> C.Expression -> Either TypeError T.Type
inferType _ (C.IntLiteral offset _) = Right T.BuiltInInt 
inferType _ (C.StringLiteral offset _) = Right T.BuiltInString
inferType scope (C.Name offset name) = DEC.maybeToRight 
  (CannotInferType offset)
  (fmap toTypedType (C.scopeVariableType scope name))

toTypedType (C.BuiltInInt _) = T.BuiltInInt
toTypedType (C.BuiltInString _) = T.BuiltInString
toTypedType (C.Canonical _ n) = T.TypeName n
toTypedType (C.FunctionType offset parameterType returnType) = 
  T.FunctionType (toTypedType parameterType) (toTypedType returnType)
