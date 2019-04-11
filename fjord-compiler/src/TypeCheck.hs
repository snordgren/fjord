module TypeCheck where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as DEC
import qualified Data.List as List

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

toTypedDeclaration decls (C.EnumDeclaration offset name constructors) = 
  let 
    toTypedEnumConstructor :: C.EnumConstructor -> T.EnumConstructor
    toTypedEnumConstructor (C.EnumConstructor _ s t) = T.EnumConstructor s (toTypedType t)
  in
    return $ T.EnumDeclaration name (fmap toTypedEnumConstructor constructors)

toTypedDeclaration decls (C.RecordDeclaration offset name fields) = 
  let 
    toTypedRecordField :: C.RecordField -> T.RecordField
    toTypedRecordField (C.RecordField _ s t) = T.RecordField s (toTypedType t)
      
  in 
    return $ T.RecordDeclaration name (fmap toTypedRecordField fields)

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
    List.foldr (C.FunctionType 0) returnType remainingParameters
  else
    returnType


createDeclarationScope :: [C.Declaration] -> [C.Parameter] -> C.Type -> C.Scope
createDeclarationScope moduleDeclarations parameters typ = 
  let
    parameterBindings = 
      (List.zip (fmap C.parameterName parameters) (functionParameterList typ))

    declarationBindings :: [(String, C.Type)]
    declarationBindings = 
      List.concat $ fmap bindingsOf moduleDeclarations
  in
    C.Scope (parameterBindings ++ declarationBindings)
  
bindingsOf :: C.Declaration -> [(String, C.Type)]
bindingsOf (C.RecordDeclaration offset name fields) =
  let 
    constructorRetType = 
      C.Canonical offset name
    
    fieldTypes = 
      fmap C.recordFieldType fields

    constructorType = 
      List.foldr (C.FunctionType offset) constructorRetType fieldTypes
  in 
    [(name, constructorType)]

bindingsOf (C.ValueDeclaration _ name _ t _) = [(name, t)]


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
    (fmap (\(_, t) -> t) (List.find (\(n, _) -> n == name) (C.scopeBindings scope)))
