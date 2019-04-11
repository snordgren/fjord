module TypeCheck where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as DEC
import qualified Data.List as List

import qualified AST.Resolved as R
import qualified AST.Typed as T


data TypeError 
  = WrongType Int T.Type T.Type 
  | CannotInferType Int 
  | TooManyParameters Int
  | UndefinedInScope Int
  deriving (Eq, Show)


typeCheck :: R.Module -> Either TypeError T.Module
typeCheck m = do
  declarations <- sequence (fmap (toTypedDeclaration (R.moduleDeclarations m))
    (R.moduleDeclarations m))
  return $ T.Module (R.moduleName m) declarations


toTypedDeclaration :: [R.Declaration] -> R.Declaration -> Either TypeError T.Declaration

toTypedDeclaration decls (R.EnumDeclaration offset name constructors) = 
  let 
    toTypedEnumConstructor :: R.EnumConstructor -> T.EnumConstructor
    toTypedEnumConstructor (R.EnumConstructor _ s t) = T.EnumConstructor s (toTypedType t)
  in
    return $ T.EnumDeclaration name (fmap toTypedEnumConstructor constructors)

toTypedDeclaration decls (R.RecordDeclaration offset name fields) = 
  let 
    toTypedRecordField :: R.RecordField -> T.RecordField
    toTypedRecordField (R.RecordField _ s t) = T.RecordField s (toTypedType t)
      
  in 
    return $ T.RecordDeclaration name (fmap toTypedRecordField fields)

toTypedDeclaration decls (R.ValueDeclaration offset name parameters declaredType expr) =
  let 
    typedDeclaredType = toTypedType declaredType
    scope = createDeclarationScope decls parameters declaredType
    requiredType = toTypedType (inferRequiredBody declaredType parameters)
    typedParameters = fmap (\(p, t) -> T.Parameter (R.parameterName p) (toTypedType t))
      (zip parameters (functionParameterList declaredType))
  in do
    inferredType <- inferType scope expr
    typedExpr <- toTypedExpression scope expr 
    if inferredType == requiredType then 
      Right $ T.ValueDeclaration name typedParameters typedDeclaredType typedExpr
    else
      Left $ WrongType (R.expressionOffset expr) requiredType inferredType


inferRequiredBody :: R.Type -> [R.Parameter] -> R.Type
inferRequiredBody declaredType parameters = 
  let 
    remainingParameters = drop (length parameters) (functionParameterList declaredType)
    returnType = last (functionTypeList declaredType)
  in if length remainingParameters > 0 then
    List.foldr (R.FunctionType 0) returnType remainingParameters
  else
    returnType


createDeclarationScope :: [R.Declaration] -> [R.Parameter] -> R.Type -> R.Scope
createDeclarationScope moduleDeclarations parameters typ = 
  let
    parameterBindings = 
      (List.zip (fmap R.parameterName parameters) (functionParameterList typ))

    declarationBindings :: [(String, R.Type)]
    declarationBindings = 
      List.concat $ fmap bindingsOf moduleDeclarations
  in
    R.Scope (parameterBindings ++ declarationBindings)
  
bindingsOf :: R.Declaration -> [(String, R.Type)]
bindingsOf (R.RecordDeclaration offset name fields) =
  let 
    constructorRetType = 
      R.Canonical offset name
    
    fieldTypes = 
      fmap R.recordFieldType fields

    constructorType = 
      List.foldr (R.FunctionType offset) constructorRetType fieldTypes
  in 
    [(name, constructorType)]

bindingsOf (R.ValueDeclaration _ name _ t _) = [(name, t)]


functionParameterList :: R.Type -> [R.Type]
functionParameterList t = 
  case t of 
    R.FunctionType _ parameterType returnType -> 
      parameterType : functionParameterList (returnType)
    _ -> []


functionTypeList :: R.Type -> [R.Type]
functionTypeList t =
  case t of 
    R.FunctionType _ p r -> p : functionTypeList r
    a -> [a]
    

toTypedExpression :: R.Scope -> R.Expression -> Either TypeError T.Expression
toTypedExpression _ (R.IntLiteral _ value) = 
  Right $ T.IntLiteral value

toTypedExpression _ (R.StringLiteral _ value) = 
  Right $ T.StringLiteral value

toTypedExpression scope (R.Name n s) = do
  t <- scopeVariableType scope n s
  return $ T.Name s (toTypedType t)
  
toTypedExpression scope (R.Addition _ a b) = do
  typedA <- toTypedExpression scope a
  typedB <- toTypedExpression scope b
  return $ T.Addition typedA typedB

toTypedExpression scope (R.Apply _ a b) = do 
  typedA <- toTypedExpression scope a
  typedB <- toTypedExpression scope b
  return $ T.Apply typedA typedB


inferType :: R.Scope -> R.Expression -> Either TypeError T.Type
inferType _ (R.IntLiteral offset _) = 
  Right T.BuiltInInt 

inferType _ (R.StringLiteral offset _) = 
  Right T.BuiltInString

inferType scope (R.Name offset name) = 
  fmap toTypedType (scopeVariableType scope offset name)

inferType scope (R.Addition offset a b) = do
  inferA <- inferType scope a
  inferB <- inferType scope b
  if inferA == inferB then 
    Right $ inferA 
  else 
    Left $ WrongType (R.expressionOffset b) inferA inferB
    
inferType scope (R.Apply offset a b) = do
  inferA <- inferType scope a
  inferB <- inferType scope b
  case inferA of 
    T.FunctionType param ret -> Right ret
    _ -> Left $ CannotInferType offset


toTypedType (R.BuiltInInt _) = T.BuiltInInt
toTypedType (R.BuiltInString _) = T.BuiltInString
toTypedType (R.Canonical _ n) = T.TypeName n
toTypedType (R.FunctionType offset par ret) = 
  T.FunctionType (toTypedType par) (toTypedType ret)


scopeVariableType :: R.Scope -> Int -> String -> Either TypeError R.Type
scopeVariableType scope offset name = 
  DEC.maybeToRight (UndefinedInScope offset)
    (fmap (\(_, t) -> t) (List.find (\(n, _) -> n == name) (R.scopeBindings scope)))
