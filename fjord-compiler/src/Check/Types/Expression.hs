{-# LANGUAGE RankNTypes #-}
module Check.Types.Expression where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Check.Scope
import Check.Types.InferUnique
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U


type UseCountM = 
  --StateT (Map.Map String Int) (Either TypeError)
  ExceptT TypeError (State [(String, Int)])


toTypedExpression 
  :: U.Scope
  -> Maybe U.Type 
  -> Common.Uniqueness 
  -> U.Expression 
  -> UseCountM T.Expression
toTypedExpression scope expectType expectUniq expr =
  case expr of 
    U.Apply _ a b ->
      do 
        typedA <- toTypedExpression scope Nothing expectUniq a
        typedB <- toTypedExpression scope Nothing expectUniq b
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
    
        toTypedPattern :: U.Pattern -> UseCountM T.Pattern
        toTypedPattern (U.Pattern offset ctor vars retExpr) = do
          (ctorType, uniq, orig) <- eitherToUseCountM $ scopeVariableType scope offset ctor 
          let patScope = createPatternScope ctorType vars scope
          types <- eitherToUseCountM $ Monad.sequence $ fmap (toTypedType patScope uniq) $ fnParamList ctorType
          let mergedVars = List.zip vars types
          typedRetExpr <- toTypedExpression patScope expectType expectUniq retExpr
          return $ T.Pattern ctor mergedVars typedRetExpr
      in do
        typedPatterns <- Monad.sequence $ fmap toTypedPattern patterns
        typedSourceExpression <- toTypedExpression scope Nothing expectUniq expression
        return $ T.Case typedSourceExpression typedPatterns

    U.IntLiteral _ value -> 
      return $ T.IntLiteral value expectUniq

    U.Lambda offset name expr ->
      do
        t <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing expected type") expectType
        parT <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing parameter type") 
          (parameterType t)
        retT <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing return type") (returnType t)
        let lambdaPar = (name, parT, Common.NonUnique, Common.SameModule)
        let lambdaScope = U.Scope (lambdaPar : U.scopeValues scope) (U.scopeTypes scope) []
        exprT <- toTypedExpression lambdaScope (Just retT) expectUniq expr
        typedT <- eitherToUseCountM $ toTypedType scope Common.NonUnique t
        return $ T.Lambda name typedT exprT
      
    U.Name offset s -> 
      let 
        findUseCount =
          List.find (\(name, _) -> s == name)

        removeUseCount :: [(String, Int)] -> [(String, Int)]
        removeUseCount =
          List.filter (\(name, _) -> s /= name)

        updateUseCount :: [(String, Int)] -> Either TypeError [(String, Int)]
        updateUseCount useCounts =
          let
            newUseCount = 
              1 + (Maybe.fromMaybe 0 $ fmap (\(_, a) -> a) $ findUseCount useCounts)
          in
            if newUseCount > 1 then
              Left $ TooManyUsages offset s
            else
              return ((s, newUseCount) : (removeUseCount useCounts))
      in 
        do
          (t, uniq, orig) <- eitherToUseCountM $ scopeVariableType scope offset s
          typedT <- eitherToUseCountM $ toTypedType scope uniq t
          if uniq == Common.Unique then
            do
              useCounts <- get
              newUseCounts <- eitherToUseCountM $ updateUseCount useCounts
              put newUseCounts
          else 
            return ()
          return $ T.Name s typedT uniq orig

    U.Operator offset name a b -> 
      do
        (opType, uniq, orig) <- eitherToUseCountM $ scopeVariableType scope offset name 
        opTypeT <- eitherToUseCountM $ toTypedType scope uniq opType
        typedA <- toTypedExpression scope expectType expectUniq a
        typedB <- toTypedExpression scope expectType expectUniq b
        return $ T.Operator name opTypeT typedA typedB

    U.RecUpdate _ target updates ->
      do
        typedTarget <- toTypedExpression scope expectType expectUniq target
        typedUpdates <- Monad.sequence $ fmap (typedFieldUpdate scope) updates
        return $ T.RecUpdate typedTarget typedUpdates

    U.StringLiteral _ value -> 
      return $ T.StringLiteral value expectUniq

    U.Tuple offset values -> 
      do
        -- TODO Propagate types here. 
        typedValues <- Monad.sequence $ fmap (toTypedExpression scope Nothing expectUniq) values
        uniqValues <- eitherToUseCountM $ Monad.sequence $ fmap (inferExprUniq scope) values
        uniq <- eitherToUseCountM $ resolveTupleUniq offset uniqValues
        return $ T.Tuple uniq typedValues


eitherToUseCountM :: Either TypeError b -> UseCountM b 
eitherToUseCountM e = 
  ExceptT $ return e

        
typedFieldUpdate 
  :: U.Scope 
  -> U.FieldUpdate 
  -> UseCountM T.FieldUpdate
typedFieldUpdate scope a = 
  let 
    name = 
      U.fieldUpdateName a

    exprT :: UseCountM T.Expression
    exprT = 
      toTypedExpression scope Nothing Common.Unique $ U.fieldUpdateExpression a
  in
    fmap (\e -> T.FieldUpdate name e) exprT


toTypedType :: U.Scope -> Common.Uniqueness -> U.Type -> Either TypeError T.Type
toTypedType scope uniq a =
  case a of 
    U.FunctionType _ par ret ->
      do
        parT <- toTypedType scope Common.NonUnique par
        retT <- toTypedType scope Common.Unique ret
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
      return $ T.BuiltInInt uniq

    U.TypeName offset "String" -> 
      return $ T.BuiltInString uniq

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


runUseCounting 
  :: forall a . 
     Int
  -> U.Scope
  -> UseCountM T.Expression
  -> Either TypeError T.Expression
runUseCounting offset scope e =
  let 
    uniqueValues =
      List.filter (\(_, _, uniq, _) -> uniq == Common.Unique) $ U.scopeValues scope

    initialMap =
      fmap (\(name, _, _, _) -> (name, 0)) uniqueValues

    (eith, finalMap) = 
      runState (runExceptT e) initialMap

    tooFewUsages = 
      List.find (\(name, useCount) -> useCount <= 0) finalMap
  in
    case tooFewUsages of
      Just (x, useCount) -> 
        Left $ TooFewUsages offset x

      Nothing -> 
        eith
  