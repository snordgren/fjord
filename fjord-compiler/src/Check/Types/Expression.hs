{-# LANGUAGE Strict, RankNTypes #-}
module Check.Types.Expression where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Functor.Identity
import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Check.Scope
import Check.Types.Common
import Check.Types.Infer
import Check.Types.Types
import Check.Types.UseCounter (UseCounter (UseCounter))
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified Check.Types.UseCounter as UseCounter

type UseCountM = 
  ExceptT TypeError (State [UseCounter])

toTypedExpression 
  :: U.Scope
  -> Maybe U.Type 
  -> Maybe Common.Uniqueness 
  -> U.Expression 
  -> UseCountM T.Expression
toTypedExpression scope expectType expectUniq expr =
  case expr of 
    U.Apply offset a b ->
      do
        typedA <- toTypedExpression scope Nothing (Just Common.Unique) a
        let parUniq = T.parTypeUniq $ T.concreteType $ T.expressionType typedA 
        typedB <- toTypedExpression scope Nothing (Just parUniq) b
        let parT = T.expressionType typedB
        let exprType = T.rewritePolyType (T.expressionType typedA) parT
        let reqParT = T.parType exprType
        if reqParT == parT then
          case exprType of 
            T.FunctionType uniq param ret -> 
              return $ T.Apply typedA typedB
  
            T.LinearFunctionType par ret -> 
              return $ T.Apply typedA typedB
  
            _ -> 
              useCountM $ Left $ CannotInferType offset $ "cannot infer function type " ++ show exprType
        else 
          useCountM $ Left $ WrongType offset reqParT parT

    U.Case offset expression patterns ->
      let 
        createPatternScope constructorType variables scope = 
          let
            variableTypes = 
              fnParamList constructorType

            bindings = 
              fmap (\(a, b) -> (a, b, Common.Unique, Common.InFunction)) $ 
                List.zip variables variableTypes

            newScope = 
              U.Scope bindings [] []
          in
            mergeScope newScope scope
    
        toTypedPattern :: U.Pattern -> UseCountM T.Pattern
        toTypedPattern (U.Pattern offset ctor vars retExpr) = do
          (ctorType, uniq, orig) <- useCountM $ scopeVariableType scope offset ctor 
          let patScope = createPatternScope ctorType vars scope
          types <- useCountM $ Monad.sequence $ fmap (toTypedType patScope uniq) $ fnParamList ctorType
          let mergedVars = List.zip vars types
          typedRetExpr <- toTypedExpression patScope expectType expectUniq retExpr
          return $ T.Pattern ctor mergedVars typedRetExpr
      in do
        typedPatterns <- Monad.sequence $ fmap toTypedPattern patterns
        typedSourceExpression <- toTypedExpression scope Nothing expectUniq expression
        return $ T.Case typedSourceExpression typedPatterns

    U.IntLiteral _ value -> 
      case expectUniq of 
        Just uniq ->
          return $ T.IntLiteral value uniq

        Nothing ->
          useCountM $ Left $ CannotInferType (U.expressionOffset expr) "cannot infer int uniqueness"

    U.Lambda offset name expr ->
      createLambda offset name scope expectUniq Common.NonUnique expectType expr T.Lambda
      
    U.Name offset s -> 
      let 
        findUseCount =
          List.find (\a -> s == (UseCounter.name a))

        removeUseCount :: [UseCounter] -> [UseCounter]
        removeUseCount =
          List.filter (\a -> s /= (UseCounter.name a))

        updateUseCount :: [UseCounter] -> Either TypeError [UseCounter]
        updateUseCount useCounts =
          let
            useCounter =
              Maybe.fromMaybe (UseCounter.for s) $ findUseCount useCounts
          in
            if UseCounter.isUsedLinearly useCounter then
              Left $ TooManyUsages offset s
            else 
              case expectUniq of 
                Just uniq ->
                  case uniq of 
                    Common.Unique -> 
                      return ((UseCounter.markUsedLinearly useCounter) : (removeUseCount useCounts))
                    
                    _ ->
                      return useCounts
                
                _ ->
                  return useCounts


        -- The uniqueness that we are going to use.
        -- If a shared value is expected, we attempt to share it.
        deriveUseUniq :: Common.Uniqueness -> Common.Uniqueness
        deriveUseUniq uniq = 
          case expectUniq of 
            Just Common.NonUnique -> 
              Common.NonUnique

            _ ->
              uniq
      in 
        do
          (t, uniq, orig) <- useCountM $ scopeVariableType scope offset s
          let useUniq = deriveUseUniq uniq
          typedT <- useCountM $ toTypedType scope useUniq t
          -- If the value is unique, update its use counter, and generate an
          -- error if it has been used uniquely before.  
          if uniq == Common.Unique then
            do
              useCounts <- get
              newUseCounts <- useCountM $ updateUseCount useCounts
              put newUseCounts
          else 
            return ()
          return $ T.Name s typedT useUniq orig

    U.Operator offset name a b -> 
      do
        (opType, uniq, intro) <- useCountM $ scopeVariableType scope offset name 
        opTypeT <- useCountM $ toTypedType scope uniq opType
        let paramAUniq = T.parTypeUniq $ opTypeT
        let paramBUniq = T.parTypeUniq $ T.returnType opTypeT
        typedA <- toTypedExpression scope expectType (Just paramAUniq) a
        typedB <- toTypedExpression scope expectType (Just paramBUniq) b
        return $ T.Operator name opTypeT typedA typedB intro

    U.RecUpdate _ target updates ->
      do
        typedTarget <- toTypedExpression scope expectType expectUniq target
        typedUpdates <- Monad.sequence $ fmap (typedFieldUpdate scope) updates
        return $ T.RecUpdate typedTarget typedUpdates

    U.StringLiteral _ value -> 
      case expectUniq of 
        Just uniq -> 
          return $ T.StringLiteral value uniq

        Nothing ->
          useCountM $ Left $ CannotInferType (U.expressionOffset expr) 
            "cannot infer uniqueness of string value"

    U.Tuple offset values ->
      if length values == 0 then
        case expectUniq of 
          Just uniq -> 
            return $ T.Tuple uniq []

          Nothing ->
            useCountM $ Left $ CannotInferType (U.expressionOffset expr)
              "cannot infer uniqueness of empty tuple"
      else 
        case expectUniq of 
          Just uniq -> 
            do
              typedValues <- Monad.sequence $ fmap (toTypedExpression scope Nothing expectUniq) values
              return $ T.Tuple uniq typedValues

          Nothing -> 
            useCountM $ Left $ CannotInferType (U.expressionOffset expr)
              "cannot infer tuple uniqueness"

    U.UniqueLambda offset name expr -> 
      createLambda offset name scope expectUniq Common.Unique expectType expr T.UniqueLambda

createLambda 
  :: Int
  -> String
  -> U.Scope
  -> Maybe Common.Uniqueness
  -> Common.Uniqueness 
  -> Maybe U.Type
  -> U.Expression
  -> (String -> T.Type -> T.Expression -> T.Expression) 
  -> UseCountM T.Expression
createLambda offset name scope expectUniq parUniq expectType expr f =
  let
    unableToInfer s t = 
      useCountM $ Combinators.maybeToRight (CannotInferType offset s) t
  in 
    do
      t <- unableToInfer "missing expected type" expectType
      parT <- unableToInfer "missing parameter type" (parameterType t)
      retT <- unableToInfer "missing return type" (returnType t)
      let lambdaPar = (name, parT, parUniq, Common.InFunction)
      let lambdaScope = U.Scope (lambdaPar : U.scopeValues scope) (U.scopeTypes scope) []
      exprT <- toTypedExpression lambdaScope (Just retT) expectUniq expr
      typedT <- useCountM $ toTypedType scope Common.NonUnique t
      return $ f name typedT exprT


useCountM :: Either TypeError b -> UseCountM b 
useCountM e = 
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
      toTypedExpression scope Nothing (Just Common.Unique) $ U.fieldUpdateExpression a
  in
    fmap (\e -> T.FieldUpdate name e) exprT


runUseCounting 
  :: forall a . 
     Int
  -> U.Scope
  -> UseCountM T.Expression
  -> Either TypeError T.Expression
runUseCounting offset scope e =
  let 
    uniqueValues =
      List.filter (\(_, _, uniq, orig) -> uniq == Common.Unique && orig == Common.InFunction) 
        $ U.scopeValues scope

    initialMap =
      fmap (\(name, _, _, _) -> UseCounter.for name) uniqueValues

    (eith, finalMap) = 
      runState (runExceptT e) initialMap

    tooFewUsages = 
      List.find (\a -> not $ UseCounter.isUsedLinearly a) finalMap
  in
    if Either.isRight eith then
      case tooFewUsages of
        Just a -> 
          Left $ TooFewUsages offset (UseCounter.name a)

        Nothing -> 
          eith
    else
      eith