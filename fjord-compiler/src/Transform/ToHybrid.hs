module Transform.ToHybrid (
  transformModule
) where

import Control.Monad.State.Lazy
import Data.Maybe
import qualified Control.Monad as Monad
import qualified Data.List as List

import qualified AST.Hybrid as H
import qualified AST.Typed as T

transformModule :: T.Module -> H.Source
transformModule m = 
  let 
    decls = List.concat $ fmap transformDeclaration (T.moduleDeclarations m)
  in
    H.Source (T.moduleName m) decls


transformDeclaration :: T.Declaration -> [H.Definition]
transformDeclaration (T.EnumDeclaration name constructors) = 
  if (List.length constructors) == 0 then
    error "constructor length should be greater than 0"
  else let 
    enumType :: H.Type
    enumType = transformType $ T.TypeName name

    enumTagType = H.BuiltInInt
    
    ctorTagName :: T.EnumConstructor -> String
    ctorTagName (T.EnumConstructor n _) = "$Tag" ++ n

    ctorTag :: (T.EnumConstructor, Integer) -> H.Definition
    ctorTag (ctor, ix) = 
      H.ValueDefinition (ctorTagName ctor) enumTagType $ H.IntLiteral ix

    ctorParameters :: T.Type -> [T.Type]
    ctorParameters (T.FunctionType a b) = a : (ctorParameters b)
    ctorParameters _ = []

    ctorFun :: T.EnumConstructor -> H.Definition
    ctorFun ctor =
      let 
        ctorParameterTypes = fmap transformType $ ctorParameters $ T.enumConstructorType ctor
        ctorParameterCount = List.length ctorParameterTypes
        ctorParameterH = fmap (\(t, s) -> (("_" ++ (show s)), t)) $ zip ctorParameterTypes [0..]
        ctorName = T.enumConstructorName ctor
        ctorParameterExprs = fmap (\(s, t) -> H.Read t s) ctorParameterH 
        ctorBody = H.Immutable $ H.Array ((H.Read enumTagType $ (ctorTagName ctor)) : ctorParameterExprs)
      in 
        if ctorParameterCount == 0 then
          H.ValueDefinition ctorName enumType ctorBody
        else
          H.FunctionDefinition ctorName ctorParameterH enumType $ H.SimpleFunctionBody ctorBody

    tags = fmap ctorTag $ zip constructors [1..]
    functions = fmap ctorFun constructors
  in tags ++ functions

transformDeclaration (T.RecordDeclaration name fields) = 
  if length fields == 0 then
    error "zero-field records are not yet implemented"
  else let
    tupleRecordField f = (T.recordFieldName f, transformType (T.recordFieldType f))
    parameters = fmap tupleRecordField fields
    returnType = transformType (T.TypeName name)
    objName = "_a"
    readObj = H.Read returnType objName 
    decls = [(objName, returnType)]
    allocStmt = H.Assign objName $ H.Allocate returnType
    initStmts = fmap (\(name, t) -> H.Mutate readObj name $ H.Read t name) parameters
    retStmt = H.Return $ H.Read returnType objName
    body = H.BlockFunctionBody $ H.Block decls (allocStmt : initStmts ++ [retStmt])
  in
    [
      H.FunctionDefinition name parameters returnType body
    ]

transformDeclaration (T.ValueDeclaration name parameters typ expr) = 
  let
    bodyParameters :: H.Expression -> [(String, H.Type)]
    bodyParameters (H.Lambda p b) = p ++ (bodyParameters b)
    bodyParameters _ = []

    realBody :: H.Expression -> H.Expression
    realBody (H.Lambda _ a) = realBody a
    realBody a = a

    findReturnType (T.FunctionType _ a) = findReturnType a
    findReturnType a = a

    (transformedExpression, hiddenParams) = runState (transformExpression expr) 0
    transformedParameters = fmap (\(T.Parameter s t) -> (s, transformType t)) parameters
    returnType = transformType $ findReturnType typ
    functionBody = H.SimpleFunctionBody (realBody transformedExpression)

    allParameters :: [(String, H.Type)]
    allParameters = transformedParameters ++ (bodyParameters transformedExpression)
  in if (List.length allParameters) == 0 then
    [
      H.ValueDefinition name (transformType typ) transformedExpression
    ]
  else 
    [
      H.FunctionDefinition name allParameters returnType functionBody
    ]


transformExpression :: T.Expression -> State Int H.Expression
transformExpression (T.Addition a b) = do
  ta <- transformExpression a
  tb <- transformExpression b
  return $ H.Addition (transformType $ T.expressionType a) ta tb

transformExpression (T.Apply a b) = 
  let 
    rootFunction :: T.Expression -> Maybe T.Expression
    rootFunction e = 
      case e of 
        T.Apply a b -> Just $ fromMaybe a (rootFunction a)
        _ -> Nothing

    functionParameterList :: T.Type -> [T.Type]
    functionParameterList (T.FunctionType a b) = [a] ++ functionParameterList b 
    functionParameterList _ = []

    parametersOfApply :: T.Expression -> [T.Expression]
    parametersOfApply e = 
      case e of 
        T.Apply a b -> (parametersOfApply a) ++ [b]
        _ -> []

    mkMissingParam :: Int -> (H.Type, Int) -> H.Expression
    mkMissingParam m (t, n) = H.Read t ("_" ++ (show (n + m)))
    
    rootF = fromMaybe a $ rootFunction a
    allParameters = functionParameterList (T.expressionType rootF)
    requiredArgumentCount = List.length allParameters
    passedParameters = parametersOfApply (T.Apply a b)
    passedParameterCount = List.length passedParameters
    missingParameters = fmap transformType $ drop passedParameterCount allParameters
    hiddenParamCount = requiredArgumentCount - passedParameterCount
    missingParamIx = List.zip missingParameters [0..(hiddenParamCount - 1)]
    missingParamArr = fmap (\(t, n) -> ("_" ++ (show n), t)) missingParamIx
  in do
    transformedParameters <- Monad.sequence (fmap transformExpression passedParameters)
    transformedRootF <- transformExpression rootF
    hiddenParamStartN <- get
    let hiddenParams = fmap (mkMissingParam hiddenParamStartN) missingParamIx
    put (hiddenParamStartN + (List.length hiddenParams))
    return (
      if hiddenParamCount == 0 then 
        H.Invoke transformedRootF (transformedParameters ++ hiddenParams)
      else 
        H.Lambda missingParamArr $ H.Invoke transformedRootF (transformedParameters ++ hiddenParams))

transformExpression (T.Case sourceExpression patterns) = 
  let 
    sourceExpressionType = transformType $ T.expressionType sourceExpression
    targetN = "target"
    tagN = "tag"
    declarations = 
      [
        (tagN, H.BuiltInInt),
        (targetN, sourceExpressionType)
      ]
    
    assignments e = 
      [
        H.Assign targetN e,
        H.Assign tagN $ H.ArrayAccess (H.Read sourceExpressionType targetN) (H.IntLiteral 0)
      ]

    readTag = H.Read H.BuiltInInt targetN

    caseStatementFor :: T.Pattern -> State Int (H.Expression, H.Statement)
    caseStatementFor (T.Pattern ctor variables returnExpression) = 
      do
        transformedReturnExpression <- transformExpression returnExpression
        return $ ((H.Equals readTag $ (H.Read H.BuiltInInt ("$Tag" ++ ctor))),
          (H.Return transformedReturnExpression))
  in 
    do
      transformedSourceExpression <- transformExpression sourceExpression
      caseStatements <- Monad.sequence $ fmap caseStatementFor patterns
      let ifStatement = H.If caseStatements Nothing
      return $ H.IIFE $ 
        H.Block declarations ((assignments transformedSourceExpression) ++ [ifStatement])
        

transformExpression (T.IntLiteral n) = 
  return $ H.IntLiteral n

transformExpression (T.Lambda variable variableType body) = 
  let 
    nestedVariables :: T.Expression -> [(String, H.Type)]
    nestedVariables (T.Lambda a b c) = (a, transformType b) : (nestedVariables c)
    nestedVariables _ = []

    lambdaBody (T.Lambda _ _ b) = lambdaBody b
    lambdaBody a = a

    retVariables = (variable, transformType variableType) : (nestedVariables body)
  in 
    do
      transformedBody <- transformExpression (lambdaBody body)
      return $ H.Lambda retVariables transformedBody

transformExpression (T.Name a t) = 
  return $ H.Read (transformType t) a

transformExpression (T.RecordUpdate sourceExpression fieldUpdates) = 
  let
    updateFieldName = "_m"
    updateFieldType = transformType (T.expressionType sourceExpression)
    readUpdateField = H.Read updateFieldType updateFieldName
    retStmt = H.Return readUpdateField

    transformFieldUpdate :: T.FieldUpdate -> State Int [H.Statement]
    transformFieldUpdate (T.FieldUpdate name expression) = do
      transformedExpression <- transformExpression expression
      return $ 
        [
          H.Mutate readUpdateField name transformedExpression
        ]
  in do
    transformedSourceExpression <- transformExpression sourceExpression
    transformedFieldUpdates <- Monad.sequence $ fmap transformFieldUpdate fieldUpdates
    let assignStmt = H.Assign updateFieldName transformedSourceExpression
    let statements = [assignStmt] ++ (List.concat $ transformedFieldUpdates) ++ [retStmt]
    return $ H.IIFE $ H.Block [(updateFieldName, updateFieldType)] statements

transformExpression (T.StringLiteral s) = 
  return $ H.StringLiteral s


transformType :: T.Type -> H.Type
transformType (T.BuiltInInt) = H.BuiltInInt
