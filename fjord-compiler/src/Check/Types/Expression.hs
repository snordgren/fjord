{-# LANGUAGE RankNTypes, ScopedTypeVariables, Strict #-}
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

import AST.Common (Type (..), compareTypEq)
import AST.Scope
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
  ExceptT TypeErrorAt (State (Int, [UseCounter]))

toTypedExpression 
  :: Scope
  -> Maybe Type 
  -> U.Expression 
  -> UseCountM T.Expression
toTypedExpression scope expectType expr =
  case expr of 
    U.Apply offset a b ->
      do
        typedA <- toTypedExpression scope Nothing a
        typedB <- toTypedExpression scope Nothing b
        let parT = T.expressionType typedB
        let exprType = T.unifyTypes scope (T.expressionType typedA) $ T.concreteType parT
        let reqParT = T.parType exprType
        if compareTypEq reqParT parT then
          case T.concreteType $ exprType of 
            FunctionType pos param ret -> 
              return $ T.Apply typedA typedB (T.returnType exprType)
  
            _ -> 
              useCountM $ Left (offset, "cannot infer function type " ++ show exprType)
        else 
          useCountM $ Left (offset, 
            "expression has type " ++ (show parT) ++ ", expected " ++ (show reqParT))

    U.Case offset expr patterns ->
      do
        typedSrcExpr <- toTypedExpression scope Nothing expr
        let mkTypedPattern = toTypedPattern scope expr expectType
        typedPatterns <- traverse mkTypedPattern patterns
        return $ T.Case typedSrcExpr typedPatterns

    U.IntLiteral _ value -> 
      return $ T.IntLiteral value

    U.Let offset name val ret -> 
      do
        typedVal <- toTypedExpression scope Nothing val
        valType <- useCountM $ typeOf scope val
        let letScope = mkScopeFromValues [(name, valType, Common.InFunction, [])]
        let useScope = mergeScope letScope scope
        (typeVarCounter, useCounters) <- get
        put (typeVarCounter, (UseCounter.for name) : useCounters)
        typedRet <- toTypedExpression useScope expectType ret
        return $ T.Let name typedVal typedRet
 
    U.Name offset s -> 
      let 
        findUseCount useCounters =
          List.find (\a -> s == (UseCounter.name a)) useCounters

        removeUseCount :: [UseCounter] -> [UseCounter]
        removeUseCount =
          List.filter (\a -> s /= (UseCounter.name a))

        updateUseCount :: [UseCounter] -> Either TypeErrorAt [UseCounter]
        updateUseCount useCounts =
          case findUseCount useCounts of 
            Just useCounter -> 
              if UseCounter.isUsedLinearly useCounter then
                Left (offset, "too many usages of " ++ (UseCounter.name useCounter))
              else 
                return ((UseCounter.markUsedLinearly useCounter) : (removeUseCount useCounts))
            Nothing -> return useCounts
      in 
        do
          (t, orig, implicits) <- useCountM $ scopeVariableType scope offset s
          -- If the value is unique, update its use counter, and generate an
          -- error if it has been used uniquely before.  
          (typeVarCounter, useCounts) <- get
          newUseCounts <- useCountM $ updateUseCount useCounts
          put (typeVarCounter, newUseCounts)
          renamedT <- renameTypeVars scope t
          return $ T.Name s renamedT orig

    U.RecAccess offset fieldName target -> 
      do
        typedTarget <- toTypedExpression scope Nothing target
        fieldType <- useCountM $ findRecordAccessType offset scope fieldName $ T.expressionType typedTarget
        return $ T.RecAccess fieldName fieldType typedTarget

    U.RecUpdate _ target updates ->
      do
        typedTarget <- toTypedExpression scope expectType target
        typedUpdates <- traverse (typedFieldUpdate scope) updates
        return $ T.RecUpdate typedTarget typedUpdates

    U.StringLiteral _ value -> 
      return $ T.StringLiteral value
            
    U.Tuple offset values ->
      if length values == 0 then
        return $ T.Tuple []
            
      else 
        do
          typedValues <- traverse (toTypedExpression scope Nothing) values
          return $ T.Tuple typedValues


useCountM :: Either TypeErrorAt b -> UseCountM b 
useCountM e = 
  ExceptT $ return e

        
typedFieldUpdate 
  :: Scope 
  -> U.FieldUpdate 
  -> UseCountM T.FieldUpdate
typedFieldUpdate scope a = 
  let 
    name = 
      U.fieldUpdateName a

    exprT :: UseCountM T.Expression
    exprT = 
      toTypedExpression scope Nothing $ U.fieldUpdateExpression a
  in
    fmap (\e -> T.FieldUpdate name e) exprT


runUseCounting 
  :: forall a . 
     Int
  -> Scope
  -> UseCountM T.Expression
  -> Either TypeErrorAt T.Expression
runUseCounting offset scope e =
  let 
    uniqueValues =
      List.filter (\(_, typ, orig, implicits) -> orig == Common.InFunction) 
        $ scopeValues scope

    initialMap =
      fmap (\(name, _, _, _) -> UseCounter.for name) uniqueValues

    (eith, (_, finalMap)) = 
      runState (runExceptT e) (0, initialMap)

    tooFewUsages = 
      List.find (\a -> not $ UseCounter.isUsedLinearly a) finalMap
  in
    if Either.isRight eith then
      case tooFewUsages of
        Just a -> 
          Left (offset, "too few usages of " ++ (UseCounter.name a))

        Nothing -> 
          eith
    else
      eith

 
toTypedPattern 
  :: Scope 
  -> U.Expression
  -> Maybe Type 
  -> U.Pattern 
  -> UseCountM T.Pattern
toTypedPattern scope expr expectType (U.Pattern offset ctor vars retExpr) = 
  do
    (ctorType, orig, implicits) <- useCountM $ scopeVariableType scope offset ctor 
    realExprType <- useCountM $ typeOf scope expr
    let patSubstCtorType = Maybe.fromMaybe ctorType $ U.returnType $ U.concreteType ctorType
    let patSubstTypeVars = U.typeNamesIn ctorType
    let patSubst = findPatSubst patSubstCtorType realExprType
    let substCtorType = List.foldl' (\acc (name, subst) -> replaceTypeName name subst acc) ctorType patSubst
    let patScope = createPatternScope substCtorType vars scope
    let types = fnParamList substCtorType
    let mergedVars = List.zip vars types
    typedRetExpr <- toTypedExpression patScope expectType retExpr
    return $ T.Pattern ctor mergedVars typedRetExpr

createPatternScope :: Type -> [String] -> Scope -> Scope
createPatternScope ctorType vars scope = 
  let
    varTypes = 
      fnParamList ctorType

    bindings = 
      fmap (\(a, b) -> (a, b, Common.InFunction, [])) $ 
        List.zip vars varTypes

    newScope = 
      mkScopeFromValues bindings
    in
      mergeScope newScope scope


findPatSubst :: Type -> Type -> [(String, Type)]
findPatSubst t exprType = 
    case exprType of
      TypeApply _ exprF exprPar ->
        case U.concreteType t of 
          TypeApply _ tF tPar ->
            case tPar of 
              TypeName pos name nameType -> 
                if nameType == Common.TypeVar then
                  [(name, exprPar)]
                else
                  []
          a -> []
      _ -> []


typeOf :: Scope -> U.Expression -> Either TypeErrorAt Type
typeOf scope expr = 
  case expr of 
    U.IntLiteral offset _ ->
      return $ TypeName 0 "Int" Common.TypeRef
      
    U.Name offset name ->
      do
        (t, orig, implicits) <- scopeVariableType scope offset name
        return t
    _ -> 
      error $ "not yet implemented for " ++ show expr


replaceTypeName :: String -> Type -> Type -> Type
replaceTypeName name with target =
  let 
    next = 
      replaceTypeName name with
  in
    case target of 
      FunctionType pos par ret -> FunctionType pos (next par) $ next ret
      TupleType pos types -> TupleType pos (fmap next types)
      TypeApply pos f par -> TypeApply pos (next f) $ next par
      TypeLambda pos arg ret -> TypeLambda pos arg $ next ret
      TypeName pos typeName nameType -> 
        if name == typeName then
          with
        else
          target


renameTypeVar :: String -> UseCountM (String, String)
renameTypeVar str =
  do
    (typeVarCounter, useCounters) <- get
    put (typeVarCounter + 1, useCounters)
    return $ (str, str ++ (show typeVarCounter))


renameTypeVars :: Scope -> Type -> UseCountM Type
renameTypeVars scope t =
  let 
    typeVars = T.typeVarsIn t
    foldF acc (prev, var) = 
      T.renameTypeVar prev var acc
  in
    do
      substitutions <- traverse renameTypeVar typeVars
      let res = List.foldl' foldF t substitutions
      return res


findRecordAccessType :: Int -> Scope -> String -> Type -> Either TypeErrorAt Type
findRecordAccessType offset scope fieldName recordType =
  let 
    tryCandidate :: (String, Type, Type, Common.Origin) -> Either TypeErrorAt [Type]
    tryCandidate (name, candRecordType, candFieldType, origin) = 
      if name == fieldName && (compareTypEq recordType $ unifyTypes scope candRecordType recordType) then
        return [candFieldType]
      else
        return []
  in
    do
      alternativesM <- traverse tryCandidate $ scopeFields scope
      let alts = List.concat alternativesM
      if length alts <= 0 then
        Left (offset, "unknown field type " ++ fieldName ++ " for type " ++ (show recordType) ++ ", " ++ show (scopeFields scope))
      else
        return $ head alts
