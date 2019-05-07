module Transform.ToHybrid (
  transformModule
) where

import Control.Monad.State.Lazy
import Data.Maybe
import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.List as List

import qualified AST.Common as Common
import qualified AST.Hybrid as H
import qualified AST.Typed as T
import qualified CodeGen.NameMangling as NameMangling


transformModule :: T.Module -> H.Source
transformModule m = 
  let 
    decls = List.concat $ fmap transformDef (T.moduleDefs m)
    imports = T.moduleImports m
  in
    H.Source (T.moduleName m) (fmap transformDep $ imports) decls


transformDep :: T.Import -> H.Dependency
transformDep imp =
  H.Dependency (T.importModule imp) $ T.importPath imp


transformDef :: T.Definition -> [H.Definition]
transformDef a = 
  case a of 
    T.EnumDef name constructors -> 
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

    T.ImplicitDef name typ expr ->
      let
        bodyParams :: H.Expression -> [(String, H.Type)]
        bodyParams (H.Lambda p b) = p ++ (bodyParams b)
        bodyParams _ = []
    
        realBody :: H.Expression -> H.Expression
        realBody (H.Lambda _ a) = realBody a
        realBody a = a
    
        findReturnType (T.FunctionType _ a) = findReturnType a
        findReturnType a = a
    
        (transformedExpr, hiddenParams) = runState (transformExpr expr) 0
        returnType = transformType $ findReturnType typ
        functionBody = H.SimpleFunctionBody (realBody transformedExpr)
    
        allParams :: [(String, H.Type)]
        allParams = bodyParams transformedExpr
      in if (List.length allParams) == 0 then
        [
          H.ValueDefinition name (transformType typ) transformedExpr
        ]
      else 
        [
          H.FunctionDefinition name allParams returnType functionBody
        ]

    T.RecDef name fields ->
      if length fields == 0 then
        error "zero-field records are not yet implemented"
      else let
        tupleRecField f = 
          (T.recFieldName f, transformType (T.recFieldType f))
    
        params = fmap tupleRecField fields
        returnType = transformType (T.TypeName name)
        objName = "_a"
        readObj = H.Read returnType objName 
        decls = [(objName, returnType, Just $ H.Allocate returnType)]
        initStmts = fmap (\(name, t) -> H.Mutate readObj name $ H.Read t name) params
        retStmt = H.Return $ H.Read returnType objName
        body = H.BlockFunctionBody $ H.Block decls (initStmts ++ [retStmt])
      in
        [
          H.FunctionDefinition name params returnType body
        ]

    T.ValDef name params implicits typ expr ->
      let
        bodyParams :: H.Expression -> [(String, H.Type)]
        bodyParams (H.Lambda p b) = p ++ (bodyParams b)
        bodyParams _ = []
    
        realBody :: H.Expression -> H.Expression
        realBody (H.Lambda _ a) = realBody a
        realBody a = a

        findReturnType a = 
          case a of 
            T.FunctionType _ b -> 
              findReturnType b

            _ -> 
              a

        transformImplicit (name, typ, expr) =
          do
            exprT <- transformExpr expr
            return (name, transformType typ, Just exprT)

        exprM = 
          do
            exprT <- transformExpr expr
            implicitsT <- Monad.sequence $ fmap transformImplicit implicits
            return (exprT, implicitsT)
    
        ((transformedExpr, implicitsT), hiddenParams) = 
          runState exprM 0

        transformedParams = 
          fmap (\(T.Parameter s t) -> (s, transformType t)) params

        returnType = 
          transformType $ findReturnType typ

        functionBody = 
          if length implicits == 0 then
            H.SimpleFunctionBody (realBody transformedExpr)
          else 
           H.BlockFunctionBody $ H.Block implicitsT [H.Return $ realBody transformedExpr]
    
        allParams :: [(String, H.Type)]
        allParams = transformedParams ++ (bodyParams transformedExpr)
      in if (List.length allParams) == 0 then
        [
          H.ValueDefinition name (transformType typ) transformedExpr
        ]
      else 
        [
          H.FunctionDefinition name allParams returnType functionBody
        ]


transformExpr :: T.Expression -> State Int H.Expression

transformExpr (T.Apply a b) = 
  let 
    rootFunction :: T.Expression -> Maybe T.Expression
    rootFunction e = 
      case e of 
        T.Apply a b -> Just $ fromMaybe a (rootFunction a)
        _ -> Nothing

    fnParamList :: T.Type -> [T.Type]
    fnParamList (T.FunctionType a b) = [a] ++ fnParamList b 
    fnParamList _ = []

    parametersOfApply :: T.Expression -> [T.Expression]
    parametersOfApply e = 
      case e of 
        T.Apply a b -> (parametersOfApply a) ++ [b]
        _ -> []

    mkMissingParam :: Int -> (H.Type, Int) -> H.Expression
    mkMissingParam m (t, n) = H.Read t ("_" ++ (show (n + m)))
    
    rootF = fromMaybe a $ rootFunction a
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

transformExpr (T.Name a t origin) = 
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

transformExpr (T.Tuple values) = 
  do
    transExprs <- Monad.sequence $ fmap transformExpr values
    return $ H.Immutable $ H.Array transExprs


transformType :: T.Type -> H.Type
transformType (T.BuiltInInt) = H.BuiltInInt
