module TypeCheck where

import Control.Monad (sequence)
import qualified Data.Either.Combinators as DEC
import qualified Data.List as L

import qualified AST.Canonical as C
import qualified AST.Typed as T


data TypeError 
  = WrongType Int T.Type T.Type 
  | CannotInferType Int 
  | TooManyParameters Int
  | UndefinedInScope Int
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
    typedExpr <- toTypedExpression scope expr 
    if inferredType == requiredType then 
      Right $ T.ValueDeclaration name typedParameters typedDeclaredType typedExpr
    else
      Left $ WrongType (C.expressionOffset expr) requiredType inferredType


inferRequiredBody :: C.Type -> [C.Parameter] -> C.Type
inferRequiredBody declaredType parameters = 
  let 
    remainingParameters = drop (length parameters) (functionParameterList declaredType)
    returnType = last (functionTypeList declaredType)
  in if length remainingParameters > 0 then
    L.foldr (C.FunctionType 0) returnType remainingParameters
  else
    returnType


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
    

toTypedExpression :: C.Scope -> C.Expression -> Either TypeError T.Expression
toTypedExpression _ (C.IntLiteral _ value) = 
  Right $ T.IntLiteral value

toTypedExpression _ (C.StringLiteral _ value) = 
  Right $ T.StringLiteral value

toTypedExpression scope (C.Name n s) = do
  t <- scopeVariableType scope n s
  return $ T.Name s (toTypedType t)
  
toTypedExpression scope (C.Addition _ a b) = do
  typedA <- toTypedExpression scope a
  typedB <- toTypedExpression scope b
  return $ T.Addition typedA typedB

toTypedExpression scope (C.Apply _ a b) = do 
  typedA <- toTypedExpression scope a
  typedB <- toTypedExpression scope b
  return $ T.Apply typedA typedB


inferType :: C.Scope -> C.Expression -> Either TypeError T.Type
inferType _ (C.IntLiteral offset _) = 
  Right T.BuiltInInt 

inferType _ (C.StringLiteral offset _) = 
  Right T.BuiltInString

inferType scope (C.Name offset name) = 
  fmap toTypedType (scopeVariableType scope offset name)

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
toTypedType (C.FunctionType offset par ret) = 
  T.FunctionType (toTypedType par) (toTypedType ret)


scopeVariableType :: C.Scope -> Int -> String -> Either TypeError C.Type
scopeVariableType scope offset name = 
  DEC.maybeToRight (UndefinedInScope offset)
    (fmap C.bindingType (L.find (\a -> (C.bindingName a) == name) (C.scopeBindings scope)))
