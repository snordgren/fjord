module Check.Types.Expression where


import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List

import Check.Scope
import Check.Types.InferUnique
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


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
            variableTypes = 
              fnParamList constructorType

            bindings = 
              fmap (\(a, b) -> (a, b, Common.Unique, Common.SameModule)) $ 
                List.zip variables variableTypes

            newScope = 
              U.Scope bindings [] []
          in
            mergeScope newScope scope
    
        toTypedPattern :: U.Pattern -> Either TypeError T.Pattern
        toTypedPattern (U.Pattern offset ctor vars retExpr) = do
          (ctorType, uniq, orig) <- scopeVariableType scope offset ctor 
          let patScope = createPatternScope ctorType vars scope
          types <- Monad.sequence $ fmap (toTypedType patScope uniq) $ fnParamList ctorType
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
        let lambdaPar = (name, parT, Common.NonUnique, Common.SameModule)
        let lambdaScope = U.Scope (lambdaPar : U.scopeValues scope) (U.scopeTypes scope) []
        exprT <- toTypedExpression lambdaScope (Just retT) expr
        typedT <- toTypedType scope Common.NonUnique t
        return $ T.Lambda name typedT exprT
      
    U.Name n s -> 
      do
        (t, uniq, orig) <- scopeVariableType scope n s
        typedT <- toTypedType scope uniq t
        return $ T.Name s typedT uniq orig

    U.Operator offset name a b -> 
      do
        (opType, uniq, orig) <- scopeVariableType scope offset name 
        opTypeT <- toTypedType scope uniq opType
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

    U.Tuple offset values -> 
      do
        -- TODO Propagate types here. 
        typedValues <- Monad.sequence $ fmap (toTypedExpression scope Nothing) values
        uniqValues <- Monad.sequence $ fmap (inferExprUniq scope) values
        uniq <- resolveTupleUniq offset uniqValues
        return $ T.Tuple uniq typedValues

        
typedFieldUpdate :: U.Scope -> U.FieldUpdate -> Either TypeError T.FieldUpdate
typedFieldUpdate scope a = 
  let 
    name = U.fieldUpdateName a
  in
    fmap (\expr -> T.FieldUpdate name expr) (toTypedExpression scope Nothing $ U.fieldUpdateExpression a)


toTypedType :: U.Scope -> Common.Uniqueness -> U.Type -> Either TypeError T.Type
toTypedType scope uniq a =
  case a of 
    U.FunctionType _ par ret ->
      do
        parT <- toTypedType scope Common.NonUnique par
        retT <- toTypedType scope Common.NonUnique ret
        return $ T.FunctionType parT retT

    U.LinearFunctionType _ par ret ->
      do
        parT <- toTypedType scope Common.Unique par
        retT <- toTypedType scope Common.Unique ret
        return $ T.LinearFunctionType parT retT

    U.TupleType _ types ->
      do
        typesT <- Monad.sequence $ fmap (toTypedType scope uniq) types
        return $ T.TupleType uniq typesT
    
    U.TypeName offset "Int" -> 
      return T.BuiltInInt

    U.TypeName offset "String" -> 
      return T.BuiltInString

    U.TypeName offset n ->
      let 
        typeNames = 
          U.scopeTypes scope 

        result = 
          List.find (\(t, _) -> t == n) $ U.scopeTypes scope

        resultE = 
          Combinators.maybeToRight (UndefinedType offset n) result
      in
        fmap (\(t, _) -> T.TypeName t) resultE
