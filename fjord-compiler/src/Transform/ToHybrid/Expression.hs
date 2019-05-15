module Transform.ToHybrid.Expression (
  transformExpr,
) where
  
import Control.Monad.State.Lazy
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Transform.ToHybrid.Type (transformType)
import qualified AST.Common as Common
import qualified AST.Hybrid as H
import qualified AST.Typed as T
import qualified CodeGen.NameMangling as NameMangling

transformExpr :: T.Expression -> State Int H.Expression

transformExpr (T.Apply a b) = 
  let 
    rootFunction :: T.Expression -> Maybe T.Expression
    rootFunction e = 
      case e of 
        T.Apply a b -> Just $ Maybe.fromMaybe a (rootFunction a)
        _ -> Nothing

    fnParamList :: T.Type -> [T.Type]
    fnParamList (T.FunctionType a b) = [a] ++ fnParamList b 
    fnParamList (T.LinearFunctionType a b) = [a] ++ fnParamList b
    fnParamList _ = []

    parametersOfApply :: T.Expression -> [T.Expression]
    parametersOfApply e = 
      case e of 
        T.Apply a b -> (parametersOfApply a) ++ [b]
        _ -> []

    mkMissingParam :: Int -> (H.Type, Int) -> H.Expression
    mkMissingParam m (t, n) = H.Read t ("_" ++ (show (n + m)))
    
    rootF = Maybe.fromMaybe a $ rootFunction a
    allParams = fnParamList (T.expressionType rootF)
    reqArgCount = List.length allParams
    passedParameters = parametersOfApply (T.Apply a b)
    passedParamCount = List.length passedParameters
    missingParameters = fmap transformType $ drop passedParamCount allParams
    hiddenParamCount = reqArgCount - passedParamCount
    missingParamIx = List.zip missingParameters [0..(hiddenParamCount - 1)]
    missingParamArr = fmap (\(t, n) -> ("_" ++ (show n), t)) missingParamIx
  in do
    transformedParams <- Monad.sequence (fmap transformExpr passedParameters)
    transformedRootF <- transformExpr rootF
    hiddenParamStartN <- get
    let hiddenParams = fmap (mkMissingParam hiddenParamStartN) missingParamIx
    put (hiddenParamStartN + (List.length hiddenParams))
    return (
      if hiddenParamCount == 0 then 
        H.Invoke transformedRootF (transformedParams ++ hiddenParams)
      else 
        H.Lambda missingParamArr $ H.Invoke transformedRootF (transformedParams ++ hiddenParams))

transformExpr (T.Case sourceExpression patterns) = 
  let 
    srcExprT = transformType $ T.expressionType sourceExpression
    targetN = "target"
    tagN = "tag"
    readSrcExprS = H.Read srcExprT targetN
    decls srcExprH = 
      [
        (targetN, srcExprT, Just srcExprH),
        (tagN, H.BuiltInInt, Just $ H.ArrayAccess (H.Read srcExprT targetN) (H.IntLiteral 0))
      ]

    readTag = H.Read H.BuiltInInt targetN

    caseStatementFor :: T.Pattern -> State Int (H.Expression, H.Block)
    caseStatementFor (T.Pattern ctor vars retExpr) = 
      let 
        condition = H.Equals readTag $ (H.Read H.BuiltInInt ("$Tag" ++ ctor))
        accessFE n = H.ArrayAccess readSrcExprS $ H.IntLiteral n
        decls = 
          fmap (\((s, t), n) -> (s, transformType t, Just $ accessFE n)) $ List.zip vars [1..]
      in do
        transformedReturnExpression <- transformExpr retExpr
        let ret = H.Return transformedReturnExpression
        let block = H.Block decls [ret]
        return $ (condition, block)
  in 
    do
      transformedSrcExpr <- transformExpr sourceExpression
      caseStatements <- Monad.sequence $ fmap caseStatementFor patterns
      let ifStatement = H.If caseStatements Nothing
      return $ H.IIFE $ 
        H.Block (decls transformedSrcExpr) [ifStatement]
        

transformExpr (T.IntLiteral n) = 
  return $ H.IntLiteral n

transformExpr (T.Lambda variable variableType body) = 
  let 
    nestedVariables :: T.Expression -> [(String, H.Type)]
    nestedVariables (T.Lambda a b c) = (a, transformType b) : (nestedVariables c)
    nestedVariables _ = []

    lambdaBody (T.Lambda _ _ b) = lambdaBody b
    lambdaBody a = a

    retVariables = (variable, transformType variableType) : (nestedVariables body)
  in 
    do
      transformedBody <- transformExpr (lambdaBody body)
      return $ H.Lambda retVariables transformedBody

transformExpr (T.Name a t uniq origin) = 
  return $ case origin of 
    Common.SameModule -> H.Read (transformType t) a
    Common.OtherModule b -> H.ReadImport (transformType t) a b


transformExpr (T.Operator name opType a b) = do
  ta <- transformExpr a
  tb <- transformExpr b
  case name of 
    "+" -> return $ H.Addition (transformType $ T.expressionType a) ta tb
    _ -> 
      let
        opFunName = NameMangling.mangle name 
      in
        return $ H.Invoke (H.Read (transformType opType) opFunName) [ta, tb] 

transformExpr (T.RecUpdate sourceExpression fieldUpdates) = 
  let
    updateFieldName = "_m"
    updateFieldType = transformType (T.expressionType sourceExpression)
    readUpdateField = H.Read updateFieldType updateFieldName
    retStmt = H.Return readUpdateField

    transformFieldUpdate :: T.FieldUpdate -> State Int [H.Statement]
    transformFieldUpdate (T.FieldUpdate name expression) = do
      transformedExpr <- transformExpr expression
      return $ 
        [
          H.Mutate readUpdateField name transformedExpr
        ]
  in do
    transformedSrcExpr <- transformExpr sourceExpression
    transformedFieldUpdates <- Monad.sequence $ fmap transformFieldUpdate fieldUpdates
    let statements = (List.concat $ transformedFieldUpdates) ++ [retStmt]
    return $ H.IIFE $ H.Block [(updateFieldName, updateFieldType, Just transformedSrcExpr)] statements

transformExpr (T.StringLiteral s) = 
  return $ H.StringLiteral s

transformExpr (T.Tuple uniq values) = 
  do
    transExprs <- Monad.sequence $ fmap transformExpr values
    return $ H.Immutable $ H.Array transExprs
