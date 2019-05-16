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
import Check.Types.UseCounter (UseCounter (UseCounter))
import qualified AST.Common as Common
import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified Check.Types.UseCounter as UseCounter

type UseCountM = 
  --StateT (Map.Map String Int) (Either TypeError)
  ExceptT TypeError (State [UseCounter])


toTypedExpression 
  :: U.Scope
  -> Maybe U.Type 
  -> Common.Uniqueness 
  -> U.Expression 
  -> UseCountM T.Expression
toTypedExpression scope expectType expectUniq expr =
  case expr of 
    U.Apply _ a b ->
      let 
        parUniq :: T.Expression -> Common.Uniqueness
        parUniq typedA = 
          case T.expressionType typedA of
            T.FunctionType _ _ -> Common.NonUnique
            T.LinearFunctionType _ _ -> Common.Unique
            _ -> error "expected function value in apply"
      in 
        do 
          typedA <- toTypedExpression scope Nothing Common.NonUnique a
          typedB <- toTypedExpression scope Nothing (parUniq typedA) b
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
      typeLambda offset name scope expectUniq Common.NonUnique expectType expr T.Lambda
      
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
            else if Common.isUnique expectUniq then
              return ((UseCounter.markUsedLinearly useCounter) : (removeUseCount useCounts))
            else
              return useCounts
      in 
        do
          (t, uniq, orig) <- eitherToUseCountM $ scopeVariableType scope offset s
          typedT <- eitherToUseCountM $ toTypedType scope uniq t
          if expectUniq == Common.Unique && uniq == Common.NonUnique then
            eitherToUseCountM $ Left $ ExpectedUnique offset
          else if uniq == Common.Unique then
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
        let paramAUniq = T.typeUniq $ T.parType opTypeT
        let paramBUniq = T.typeUniq $ T.parType $ T.returnType opTypeT
        typedA <- toTypedExpression scope expectType paramAUniq a
        typedB <- toTypedExpression scope expectType paramBUniq b
        return $ T.Operator name opTypeT typedA typedB orig

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

    U.UniqueLambda offset name expr -> 
      typeLambda offset name scope expectUniq Common.Unique expectType expr T.UniqueLambda

typeLambda 
  :: Int
  -> String
  -> U.Scope
  -> Common.Uniqueness
  -> Common.Uniqueness 
  -> Maybe U.Type
  -> U.Expression
  -> (String -> T.Type -> T.Expression -> T.Expression) 
  -> UseCountM T.Expression
typeLambda offset name scope expectUniq parUniq expectType expr f =
  do
    t <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing expected type") expectType
    parT <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing parameter type") (parameterType t)
    retT <- eitherToUseCountM $ Combinators.maybeToRight (CannotInferType offset "missing return type") (returnType t)
    let lambdaPar = (name, parT, parUniq, Common.SameModule)
    let lambdaScope = U.Scope (lambdaPar : U.scopeValues scope) (U.scopeTypes scope) []
    exprT <- toTypedExpression lambdaScope (Just retT) expectUniq expr
    typedT <- eitherToUseCountM $ toTypedType scope Common.NonUnique t
    return $ f name typedT exprT


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
      fmap (\(name, _, _, _) -> UseCounter.for name) uniqueValues

    (eith, finalMap) = 
      runState (runExceptT e) initialMap

    tooFewUsages = 
      List.find (\a -> not $ UseCounter.isUsedLinearly a) finalMap
  in
    case tooFewUsages of
      Just a -> 
        Left $ TooFewUsages offset (UseCounter.name a)

      Nothing -> 
        eith
  