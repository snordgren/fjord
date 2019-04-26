module TypeCheck where

import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import qualified AST.Typed as T
import qualified AST.Untyped as U


data TypeError 
  = CannotInferType Int String
  | ImplicitNotFound Int U.Type String
  | TooManyParameters Int
  | UndefinedInScope Int
  | UndefinedType Int String
  | WrongType Int T.Type T.Type 
  deriving (Eq, Show)


typeCheck :: U.Module -> Either TypeError T.Module
typeCheck m = do
  defs <- sequence (fmap (toTypedDef (U.moduleDefs m))
    (U.moduleDefs m))
  return $ T.Module (U.moduleName m) defs


toTypedDef :: [U.Definition] -> U.Definition -> Either TypeError T.Definition
toTypedDef defs a =
  case a of 
    U.EnumDef (U.EnumDecl offset name constructors) ->
      let 
        scope = deriveModuleScope defs

        toTypedEnumConstructor (U.EnumConstructor _ s t) = 
          do
            typedT <- toTypedType scope t
            return $ T.EnumConstructor s typedT
      in do
        ctors <- Monad.sequence $ fmap toTypedEnumConstructor constructors
        return $ T.EnumDef name ctors

    U.ImplicitDef (U.ValDecl offset name implicits declType) expr -> 
      let 
        scope = createDefScope defs [] declType
        reqType = inferRequiredBody declType implicits []          
      in do
        declTypeT <- toTypedType scope declType
        reqTypeT <- toTypedType scope reqType
        inferredType <- inferType scope (Just reqTypeT) expr
        typedExpr <- toTypedExpression scope (Just reqType) expr 
        if inferredType == reqTypeT then 
          Right $ T.ImplicitDef name declTypeT typedExpr
        else
          Left $ WrongType (U.expressionOffset expr) reqTypeT inferredType


    U.RecDef (U.RecDecl offset name fields) ->
      let 
        scope = 
          deriveModuleScope defs

        toTypedRecField (U.RecField _ s t) =
          do
            typedT <- toTypedType scope t
            return $ T.RecField s typedT
      in 
        do
          fieldsT <- Monad.sequence $ fmap toTypedRecField fields
          return $ T.RecDef name fieldsT

    U.ValDef (U.ValDecl offset name implicits declType) params expr -> 
      let 
        scope :: U.Scope
        scope = 
          createDefScope defs params declType

        reqType :: U.Type
        reqType = 
          inferRequiredBody declType implicits params
        
        toTypedParam (p, t) =
          do
            typedT <- toTypedType scope t
            return $ T.Parameter (U.parameterName p) typedT

        implicitParNames :: [String]
        implicitParNames = 
          fmap U.parameterName $ take (length implicits) params


        compareTypEq :: U.Type -> U.Type -> Bool
        compareTypEq a b = 
          case a of 
            U.FunctionType _ c d -> 
              case b of 
                U.FunctionType _ e f -> 
                  (compareTypEq c e) && (compareTypEq d f)

                _ -> 
                  False

            U.TypeName _ c ->
              case b of 
                U.TypeName _ d -> 
                  c == d

                _ ->
                  False

            U.TupleType _ c ->
              case b of 
                U.TupleType _ d -> 
                  c == d

                _ ->
                  False

        findImplicitDef :: U.Type -> Maybe (String, U.Type)
        findImplicitDef t = 
          List.find (\(_, it) -> compareTypEq t it) $ U.scopeImplicits scope 

        resolveImplicit :: (String, U.Type) -> Either TypeError (String, T.Type, T.Expression)
        resolveImplicit (a, t) = 
          do
            (name, _) <- Combinators.maybeToRight (ImplicitNotFound offset t a) $ findImplicitDef t
            typedT <- toTypedType scope t
            return (a, typedT, T.Name name typedT)

      in do
        reqTypeT <- toTypedType scope reqType
        declTypeT <- toTypedType scope declType
        paramsT <- Monad.sequence $ fmap toTypedParam $ zip (drop (length implicits) params) $ fnParamList declType
        implicitsT <- Monad.sequence $ fmap resolveImplicit $ zip implicitParNames implicits
        inferredType <- inferType scope (Just reqTypeT) expr
        typedExpr <- toTypedExpression scope (Just reqType) expr 
        if inferredType == reqTypeT then 
          Right $ T.ValDef name paramsT implicitsT declTypeT typedExpr
        else
          Left $ WrongType (U.expressionOffset expr) reqTypeT inferredType


inferRequiredBody :: U.Type -> [U.Type] -> [U.Parameter] -> U.Type
inferRequiredBody declaredType implicits parameters = 
  let 
    remainingParameters = drop (length parameters) (fnParamList declaredType)
    returnType = last (functionTypeList declaredType)
  in if length remainingParameters > 0 then
    List.foldr (U.FunctionType 0) returnType remainingParameters
  else
    returnType


deriveModuleScope :: [U.Definition] -> U.Scope
deriveModuleScope defs =
  List.foldl' mergeScope (U.Scope [] [] []) $ fmap scopeContrib defs

{-|
Merge two scopes, the tightest bound (innermost) scope should come first. 
-}
mergeScope :: U.Scope -> U.Scope -> U.Scope
mergeScope a b = 
  let 
    mergedValues = U.scopeValues a ++ U.scopeValues b
    mergedTypes = U.scopeTypes a ++ U.scopeTypes b
    mergedImplicits = U.scopeImplicits a ++ U.scopeImplicits b
  in
    U.Scope mergedValues mergedTypes mergedImplicits


{-|
Derive the scope of a definition with parameters. 
-}
createDefScope :: [U.Definition] -> [U.Parameter] -> U.Type -> U.Scope
createDefScope defs parameters typ = 
  let
    parameterBindings = 
      (List.zip (fmap U.parameterName parameters) (fnParamList typ))

    defScope = 
      U.Scope parameterBindings [] []
  in
    mergeScope defScope $ deriveModuleScope defs
    

{-|
Generates the individual contribution of the definition to the module scope.
-}
scopeContrib :: U.Definition -> U.Scope
scopeContrib d =
  case d of 
    U.EnumDef (U.EnumDecl offset name constructors) -> 
      let 
        bindConstructor :: U.EnumConstructor -> (String, U.Type)
        bindConstructor c = 
          (U.enumConstructorName c, U.enumConstructorType c)

        values = 
          fmap bindConstructor constructors

        types =
          [name]
      in
        U.Scope values types []

    U.ImplicitDef (U.ValDecl offset name implicits t) _ -> 
      U.Scope [(name, t)] [] [(name, t)]

    U.RecDef (U.RecDecl offset name fields) -> 
      let 
        constructorRetType = 
          U.TypeName offset name
        
        fieldTypes = 
          fmap U.recFieldType fields
    
        constructorType = 
          List.foldr (U.FunctionType offset) constructorRetType fieldTypes

        values = 
          [(name, constructorType)]

        types = 
          [name]
      in 
        U.Scope values types []
    
    U.ValDef (U.ValDecl _ name implicits t) _ _ -> 
      let 
        values = [(filter (\a -> a /= '(' && a /= ')') name, t)]
      in
        U.Scope values [] []


fnParamList :: U.Type -> [U.Type]
fnParamList t = 
  case t of 
    U.FunctionType _ par ret -> 
      par : fnParamList ret

    _ -> 
      []


functionTypeList :: U.Type -> [U.Type]
functionTypeList t =
  case t of 
    U.FunctionType _ p r -> 
      p : functionTypeList r
      
    a -> 
      [a]
    

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
            variableTypes = fnParamList constructorType
            bindings = List.zip variables variableTypes
            newScope = U.Scope bindings [] []
          in
            mergeScope newScope scope
    
        toTypedPattern :: U.Pattern -> Either TypeError T.Pattern
        toTypedPattern (U.Pattern offset ctor vars retExpr) = do
          ctorType <- scopeVariableType scope offset ctor 
          let patScope = createPatternScope ctorType vars scope
          types <- Monad.sequence $ fmap (toTypedType patScope) $ fnParamList ctorType
          let mergedVars = List.zip vars types
          typedRetExpr <- toTypedExpression patScope expectedType retExpr
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
        let lambdaScope = U.Scope ((name, parT) : U.scopeValues scope) (U.scopeTypes scope) []
        exprT <- toTypedExpression lambdaScope (Just retT) expr
        typedT <- toTypedType scope t
        return $ T.Lambda name typedT exprT
      
    U.Name n s -> 
      do
        t <- scopeVariableType scope n s
        typedT <- toTypedType scope t
        return $ T.Name s typedT

    U.Operator offset name a b -> 
      do
        opType <- scopeVariableType scope offset name 
        opTypeT <- toTypedType scope opType
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
      do
        unT <- scopeVariableType scope offset name
        toTypedType scope unT

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


toTypedType :: U.Scope -> U.Type -> Either TypeError T.Type
toTypedType scope a =
  case a of 
    U.FunctionType _ par ret ->
      do
        parT <- toTypedType scope par
        retT <- toTypedType scope ret
        return $ T.FunctionType parT retT

    U.TupleType _ types ->
      do
        typesT <- Monad.sequence $ fmap (toTypedType scope) types
        return $ T.TupleType typesT
    
    U.TypeName offset "Int" -> 
      return T.BuiltInInt

    U.TypeName offset "String" -> 
      return T.BuiltInString

    U.TypeName offset n ->
      let 
        typeNames = 
          U.scopeTypes scope 

        result = 
          List.find ((==) n) $ U.scopeTypes scope

        resultE = 
          Combinators.maybeToRight (UndefinedType offset n) result
      in
        fmap T.TypeName resultE


scopeVariableType :: U.Scope -> Int -> String -> Either TypeError U.Type
scopeVariableType scope offset name = 
  Combinators.maybeToRight (UndefinedInScope offset)
    (fmap (\(_, t) -> t) (List.find (\(n, _) -> n == name) (U.scopeValues scope)))


parameterType :: U.Type -> Maybe U.Type
parameterType (U.FunctionType _ p _) = Just p
parameterType _ = Nothing


returnType :: U.Type -> Maybe U.Type
returnType (U.FunctionType _ _ ret) = Just ret
returnType _ = Nothing
