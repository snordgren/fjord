module TypeCheck where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import qualified AST.Resolved as R
import qualified AST.Typed as T


data TypeError 
  = WrongType Int T.Type T.Type 
  | CannotInferType Int String
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
    requiredType = inferRequiredBody declaredType parameters
    typedRequiredType = toTypedType requiredType
    typedParameters = fmap (\(p, t) -> T.Parameter (R.parameterName p) (toTypedType t))
      (zip parameters (functionParameterList declaredType))
  in do
    inferredType <- inferType scope (Just typedRequiredType) expr
    typedExpr <- toTypedExpression scope (Just requiredType) expr 
    if inferredType == typedRequiredType then 
      Right $ T.ValueDeclaration name typedParameters typedDeclaredType typedExpr
    else
      Left $ WrongType (R.expressionOffset expr) typedRequiredType inferredType


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
bindingsOf (R.EnumDeclaration offset name constructors) = 
  let 
    bindConstructor :: R.EnumConstructor -> (String, R.Type)
    bindConstructor c = (R.enumConstructorName c, R.enumConstructorType c)
  in
    fmap bindConstructor constructors

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
    

toTypedExpression :: R.Scope -> Maybe R.Type -> R.Expression -> Either TypeError T.Expression
toTypedExpression _ _ (R.IntLiteral _ value) = 
  Right $ T.IntLiteral value

toTypedExpression _ _ (R.StringLiteral _ value) = 
  Right $ T.StringLiteral value

toTypedExpression scope _ (R.Name n s) = do
  t <- scopeVariableType scope n s
  return $ T.Name s (toTypedType t)
  
toTypedExpression scope expectedType (R.Addition _ a b) = do
  typedA <- toTypedExpression scope expectedType a
  typedB <- toTypedExpression scope expectedType b
  return $ T.Addition typedA typedB

toTypedExpression scope _ (R.Apply _ a b) = do 
  typedA <- toTypedExpression scope Nothing a
  typedB <- toTypedExpression scope Nothing b
  return $ T.Apply typedA typedB

toTypedExpression scope expectedType (R.Case offset expression patterns) = 
  let 
    toTypedPattern :: R.Pattern -> Either TypeError T.Pattern
    toTypedPattern (R.Pattern offset constructor variables returnExpression) = do
      typedReturnExpression <- toTypedExpression scope expectedType returnExpression
      return $ T.Pattern constructor variables typedReturnExpression
  in do
    typedPatterns <- Monad.sequence $ fmap toTypedPattern patterns
    typedSourceExpression <- toTypedExpression scope Nothing expression
    return $ T.Case typedSourceExpression typedPatterns

toTypedExpression scope expectedType (R.Lambda offset name expr) = do
  t <- Combinators.maybeToRight (CannotInferType offset "missing expected type") expectedType
  parT <- Combinators.maybeToRight (CannotInferType offset "missing parameter type") 
    (parameterType t)
  retT <- Combinators.maybeToRight (CannotInferType offset "missing return type") (returnType t)
  let lambdaScope = R.Scope ((name, parT) : R.scopeBindings scope)
  exprT <- toTypedExpression lambdaScope (Just retT) expr
  let typedT = toTypedType t
  return $ T.Lambda name typedT exprT

toTypedExpression scope expectedType (R.RecordUpdate _ target updates) = do
  typedTarget <- toTypedExpression scope expectedType target
  typedUpdates <- Monad.sequence $ fmap (typedFieldUpdate scope) updates
  return $ T.RecordUpdate typedTarget typedUpdates


typedFieldUpdate :: R.Scope -> R.FieldUpdate -> Either TypeError T.FieldUpdate
typedFieldUpdate scope a = 
  let 
    name = R.fieldUpdateName a
  in
    fmap (\expr -> T.FieldUpdate name expr) (toTypedExpression scope Nothing $ R.fieldUpdateExpression a)


inferType :: R.Scope -> Maybe T.Type -> R.Expression -> Either TypeError T.Type
inferType _ _ (R.IntLiteral offset _) = 
  Right T.BuiltInInt 

inferType _ _ (R.StringLiteral offset _) = 
  Right T.BuiltInString

inferType scope _ (R.Name offset name) = 
  fmap toTypedType (scopeVariableType scope offset name)

inferType scope expectedType (R.Addition offset a b) = do
  inferA <- inferType scope expectedType a
  inferB <- inferType scope expectedType b
  if inferA == inferB then 
    Right $ inferA 
  else 
    Left $ WrongType (R.expressionOffset b) inferA inferB
    
inferType scope _ (R.Apply offset a b) = do
  inferA <- inferType scope Nothing a
  inferB <- inferType scope Nothing b
  case inferA of 
    T.FunctionType param ret -> Right ret
    _ -> Left $ CannotInferType offset "cannot infer function type"

inferType scope expectedType (R.Case offset expr patterns) =
  inferType scope expectedType (R.patternExpression (head patterns))

inferType scope expectedType (R.Lambda offset name expr) = 
  Combinators.maybeToRight (CannotInferType offset "cannot infer lambda type") expectedType

inferType scope expectedType (R.RecordUpdate _ target _) =
  inferType scope expectedType target

toTypedType (R.BuiltInInt _) = T.BuiltInInt
toTypedType (R.BuiltInString _) = T.BuiltInString
toTypedType (R.Canonical _ n) = T.TypeName n
toTypedType (R.FunctionType offset par ret) = 
  T.FunctionType (toTypedType par) (toTypedType ret)


scopeVariableType :: R.Scope -> Int -> String -> Either TypeError R.Type
scopeVariableType scope offset name = 
  Combinators.maybeToRight (UndefinedInScope offset)
    (fmap (\(_, t) -> t) (List.find (\(n, _) -> n == name) (R.scopeBindings scope)))


parameterType :: R.Type -> Maybe R.Type
parameterType (R.FunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: R.Type -> Maybe R.Type
returnType (R.FunctionType _ _ ret) = Just ret
returnType _ = Nothing
