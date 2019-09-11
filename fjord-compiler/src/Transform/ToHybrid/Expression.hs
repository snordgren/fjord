module Transform.ToHybrid.Expression (
  transformExpr,
) where
  
import Control.Monad.State.Lazy
import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Transform.ToHybrid.Type (transformType)
import qualified AST.Common as Common
import qualified AST.Hybrid as H
import qualified AST.Typed as T
import qualified CodeGen.NameMangling as NameMangling

{- Transform an expression from typed AST to JS-level AST. -}
transformExpr :: T.Expression -> State Int H.Expression
transformExpr expr = 
  case expr of 
    T.Apply a b -> 
      transformApply a b
    
    T.Case sourceExpression patterns -> 
      transformCase sourceExpression patterns

    T.IntLiteral n ->
      return $ H.IntLiteral n
      
    T.Let var varExpr retExpr ->
      do
        varExprT <- transformExpr varExpr
        retExprT <- transformExpr retExpr
        let blockDecls = [(var, transformType $ T.expressionType varExpr, Just varExprT)]
        return $ H.IIFE $ H.Block blockDecls [H.Return retExprT]

    T.Name a t origin ->
      return $ case origin of 
        Common.SameModule -> H.Read (transformType t) $ NameMangling.mangle a
        Common.InFunction -> H.Read (transformType t) $ NameMangling.mangle a
        Common.OtherModule b -> H.ReadImport (transformType t) (NameMangling.mangle a) b

    T.Operator expr opType a b orig ->
      do 
        readOp <- transformExpr expr
        ta <- transformExpr a
        tb <- transformExpr b
        return $ H.Invoke readOp [ta, tb] 

    T.RecAccess fieldName fieldType sourceExpression ->
      do
        srcExprT <- transformExpr sourceExpression
        return $ H.FieldAccess fieldName srcExprT

    T.RecUpdate sourceExpression fieldUpdates ->
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
        transformedFieldUpdates <- traverse transformFieldUpdate fieldUpdates
        let statements = (List.concat $ transformedFieldUpdates) ++ [retStmt]
        return $ H.IIFE $ H.Block [(updateFieldName, updateFieldType, Just transformedSrcExpr)] statements

    T.StringLiteral s -> 
      return $ H.StringLiteral s
    
    T.Tuple values -> 
      do
        transExprs <- traverse transformExpr values
        return $ H.Immutable $ H.Array transExprs


transformCase :: T.Expression -> [T.Pattern] -> State Int H.Expression
transformCase sourceExpression patterns  =
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
      caseStatements <- traverse caseStatementFor patterns
      let ifStatement = H.If caseStatements Nothing
      return $ H.IIFE $ 
        H.Block (decls transformedSrcExpr) [ifStatement]

transformApply :: T.Expression -> T.Expression -> State Int H.Expression
transformApply a b =
  let 
    rootFunction :: T.Expression -> Maybe T.Expression
    rootFunction e = 
      case e of 
        T.Apply a b -> Just $ Maybe.fromMaybe a (rootFunction a)
        _ -> Nothing

    parametersOfApply :: T.Expression -> [T.Expression]
    parametersOfApply e = 
      case e of 
        T.Apply a b -> (parametersOfApply a) ++ [b]
        _ -> []

    mkMissingParam :: Int -> (H.Type, Int) -> H.Expression
    mkMissingParam m (t, n) = H.Read t ("_" ++ (show (n + m)))
    
    rootF = Maybe.fromMaybe a $ rootFunction a
    allParams = T.fnParamList $ T.expressionType rootF
    reqArgCount = List.length allParams
    passedParameters = parametersOfApply $ T.Apply a b
    passedParamCount = List.length passedParameters
    missingParameters = fmap transformType $ drop passedParamCount allParams
    hiddenParamCount = reqArgCount - passedParamCount
    missingParamIx = List.zip missingParameters [0..(hiddenParamCount - 1)]
    missingParamArr = fmap (\(t, n) -> ("_" ++ (show n), t)) missingParamIx
  in do
    transformedParams <- traverse transformExpr passedParameters
    transformedRootF <- transformExpr rootF
    hiddenParamStartN <- get
    let hiddenParams = fmap (mkMissingParam hiddenParamStartN) missingParamIx
    put (hiddenParamStartN + (List.length hiddenParams))
    return (
      if hiddenParamCount == 0 then 
        H.Invoke transformedRootF (transformedParams ++ hiddenParams)
      else 
        H.Lambda missingParamArr $ H.Invoke transformedRootF (transformedParams ++ hiddenParams))
