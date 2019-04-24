module TypeCheck where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import qualified AST.Typed as T
import qualified AST.Untyped as U


data TypeError 
  = WrongType Int T.Type T.Type 
  | CannotInferType Int String
  | TooManyParameters Int
  | UndefinedInScope Int
  deriving (Eq, Show)


typeCheck :: U.Module -> Either TypeError T.Module
typeCheck m = do
  decls <- sequence (fmap (toTypedDef (U.moduleDefs m))
    (U.moduleDefs m))
  return $ T.Module (U.moduleName m) decls


toTypedDef :: [U.Definition] -> U.Definition -> Either TypeError T.Definition
toTypedDef decls a =
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors) ->
      let 
        toTypedEnumConstructor :: U.EnumConstructor -> T.EnumConstructor
        toTypedEnumConstructor (U.EnumConstructor _ s t) = T.EnumConstructor s (toTypedType t)
      in
        return $ T.EnumDef name (fmap toTypedEnumConstructor constructors)

    U.RecDef (U.RecDecl offset name fields) ->
      let 
        toTypedRecField :: U.RecField -> T.RecField
        toTypedRecField (U.RecField _ s t) =
          T.RecField s (toTypedType t)
          
      in 
        return $ T.RecDef name (fmap toTypedRecField fields)

    U.ValDef (U.ValDecl offset name declType) params expr -> 
      let 
        typedDeclType = toTypedType declType
        scope = createDefScope decls params declType
        requiredType = inferRequiredBody declType params
        typedRequiredType = toTypedType requiredType
        typedParams = fmap (\(p, t) -> T.Parameter (U.parameterName p) (toTypedType t))
          (zip params (functionParameterList declType))
      in do
        inferredType <- inferType scope (Just typedRequiredType) expr
        typedExpr <- toTypedExpression scope (Just requiredType) expr 
        if inferredType == typedRequiredType then 
          Right $ T.ValDef name typedParams typedDeclType typedExpr
        else
          Left $ WrongType (U.expressionOffset expr) typedRequiredType inferredType


inferRequiredBody :: U.Type -> [U.Parameter] -> U.Type
inferRequiredBody declaredType parameters = 
  let 
    remainingParameters = drop (length parameters) (functionParameterList declaredType)
    returnType = last (functionTypeList declaredType)
  in if length remainingParameters > 0 then
    List.foldr (U.FunctionType 0) returnType remainingParameters
  else
    returnType


createDefScope :: [U.Definition] -> [U.Parameter] -> U.Type -> U.Scope
createDefScope moduleDefs parameters typ = 
  let
    parameterBindings = 
      (List.zip (fmap U.parameterName parameters) (functionParameterList typ))

    declBindings :: [(String, U.Type)]
    declBindings = 
      List.concat $ fmap bindingsOf moduleDefs
  in
    U.Scope (parameterBindings ++ declBindings)
  

bindingsOf :: U.Definition -> [(String, U.Type)]
bindingsOf a = 
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors) -> 
      let 
        bindConstructor :: U.EnumConstructor -> (String, U.Type)
        bindConstructor c = (U.enumConstructorName c, U.enumConstructorType c)
      in
        fmap bindConstructor constructors

    U.RecDef (U.RecDecl offset name fields) -> 
      let 
        constructorRetType = 
          U.TypeName offset name
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        constructorType = 
          List.foldr (U.FunctionType offset) constructorRetType fieldTypes
      in 
        [(name, constructorType)]
    
    U.ValDef (U.ValDecl _ name t) _ _ -> 
        [(filter (\a -> a /= '(' && a /= ')') name, t)]


functionParameterList :: U.Type -> [U.Type]
functionParameterList t = 
  case t of 
    U.FunctionType _ parameterType returnType -> 
      parameterType : functionParameterList (returnType)
    _ -> []


functionTypeList :: U.Type -> [U.Type]
functionTypeList t =
  case t of 
    U.FunctionType _ p r -> p : functionTypeList r
    a -> [a]
    

toTypedExpression :: U.Scope -> Maybe U.Type -> U.Expression -> Either TypeError T.Expression
toTypedExpression scope expectedType expr =
  case expr of 
    U.Apply _ a b ->
      do 
        typedA <- toTypedExpression scope Nothing a
        typedB <- toTypedExpression scope Nothing b
        return $ T.Apply typedA typedB

    U.Case offset expression patterns ->
      let 
        createPatternScope constructorType variables scope = 
          let
            variableTypes = functionParameterList constructorType
            bindings = List.zip variables variableTypes
          in
            U.Scope (bindings ++ (U.scopeBindings scope))
    
        toTypedPattern :: U.Pattern -> Either TypeError T.Pattern
        toTypedPattern (U.Pattern offset ctor vars retExpr) = do
          ctorType <- scopeVariableType scope offset ctor 
          let patternScope = createPatternScope ctorType vars scope
          let mergedVars = List.zip vars $ fmap toTypedType $ functionParameterList ctorType
          typedRetExpr <- toTypedExpression patternScope expectedType retExpr
          return $ T.Pattern ctor mergedVars typedRetExpr
      in do
        typedPatterns <- Monad.sequence $ fmap toTypedPattern patterns
        typedSourceExpression <- toTypedExpression scope Nothing expression
        return $ T.Case typedSourceExpression typedPatterns

    U.IntLiteral _ value -> 
      Right $ T.IntLiteral value

    U.Lambda offset name expr ->
      do
        t <- Combinators.maybeToRight (CannotInferType offset "missing expected type") expectedType
        parT <- Combinators.maybeToRight (CannotInferType offset "missing parameter type") 
          (parameterType t)
        retT <- Combinators.maybeToRight (CannotInferType offset "missing return type") (returnType t)
        let lambdaScope = U.Scope ((name, parT) : U.scopeBindings scope)
        exprT <- toTypedExpression lambdaScope (Just retT) expr
        let typedT = toTypedType t
        return $ T.Lambda name typedT exprT
      
    U.Name n s -> 
      do
        t <- scopeVariableType scope n s
        return $ T.Name s (toTypedType t)

    U.Operator offset name a b -> 
      do
        opType <- scopeVariableType scope offset name 
        let opTypeT = toTypedType opType
        typedA <- toTypedExpression scope expectedType a
        typedB <- toTypedExpression scope expectedType b
        return $ T.Operator name opTypeT typedA typedB

    U.RecUpdate _ target updates ->
      do
        typedTarget <- toTypedExpression scope expectedType target
        typedUpdates <- Monad.sequence $ fmap (typedFieldUpdate scope) updates
        return $ T.RecUpdate typedTarget typedUpdates

    U.StringLiteral _ value -> 
      Right $ T.StringLiteral value

    U.Tuple _ values -> 
      do
        -- TODO Propagate types here. 
        typedValues <- Monad.sequence $ fmap (toTypedExpression scope Nothing) values
        return $ T.Tuple typedValues
      

typedFieldUpdate :: U.Scope -> U.FieldUpdate -> Either TypeError T.FieldUpdate
typedFieldUpdate scope a = 
  let 
    name = U.fieldUpdateName a
  in
    fmap (\expr -> T.FieldUpdate name expr) (toTypedExpression scope Nothing $ U.fieldUpdateExpression a)


inferType :: U.Scope -> Maybe T.Type -> U.Expression -> Either TypeError T.Type
inferType scope expectedType expr = 
  case expr of 
    U.Apply offset a b ->
      do
        inferA <- inferType scope Nothing a
        inferB <- inferType scope Nothing b
        case inferA of 
          T.FunctionType param ret -> Right ret
          _ -> Left $ CannotInferType offset "cannot infer function type"

    U.Case offset expr patterns -> 
      inferType scope expectedType (U.patternExpression (head patterns))

    U.IntLiteral offset _ -> 
      Right T.BuiltInInt

    U.Lambda offset name expr ->
      Combinators.maybeToRight (CannotInferType offset "cannot infer lambda type") expectedType

    U.Name offset name ->
      fmap toTypedType (scopeVariableType scope offset name)

    U.Operator offset name a b ->
      fmap (T.returnType . T.returnType) $ inferType scope expectedType (U.Name offset name)

    U.RecUpdate _ target _ ->
      inferType scope expectedType target

    U.StringLiteral offset _ -> 
      Right T.BuiltInString

    -- TODO Propagate expected type if expected type is a tuple.
    U.Tuple offset values -> 
      do
        inferredValueTypes <- Monad.sequence $ fmap (inferType scope Nothing) values
        return $ T.TupleType inferredValueTypes


toTypedType :: U.Type -> T.Type
toTypedType a =
  case a of 
    U.BuiltInInt _ -> 
      T.BuiltInInt

    U.BuiltInString _ -> 
      T.BuiltInString

    U.FunctionType _ par ret ->
      T.FunctionType (toTypedType par) (toTypedType ret)

    U.TupleType _ types ->
      T.TupleType $ fmap toTypedType types
    
    U.TypeName _ n ->
      T.TypeName n


scopeVariableType :: U.Scope -> Int -> String -> Either TypeError U.Type
scopeVariableType scope offset name = 
  Combinators.maybeToRight (UndefinedInScope offset)
    (fmap (\(_, t) -> t) (List.find (\(n, _) -> n == name) (U.scopeBindings scope)))


parameterType :: U.Type -> Maybe U.Type
parameterType (U.FunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: U.Type -> Maybe U.Type
returnType (U.FunctionType _ _ ret) = Just ret
returnType _ = Nothing
