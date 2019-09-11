{-
Transforms the high-level typed AST to a hybrid procedural-functional AST 
suitable for either lowering to C or generating JavaScript source. 
-}
module Transform.ToHybrid (
  transformModule
) where

import Control.Monad.State.Lazy
import Data.Maybe
import Debug.Trace
import qualified Control.Monad as Monad
import qualified Data.List as List

import Transform.ToHybrid.Expression (transformExpr)
import Transform.ToHybrid.Type (transformType)
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
        enumType = 
          H.TypeName name
    
        enumTagType = 
          H.BuiltInInt
        
        ctorTagName :: T.EnumConstructor -> String
        ctorTagName (T.EnumConstructor n _ _) = "$Tag" ++ n
    
        ctorTag :: (T.EnumConstructor, Integer) -> H.Definition
        ctorTag (ctor, ix) = 
          H.ValueDefinition (ctorTagName ctor) enumTagType $ H.IntLiteral ix
    
        ctorFun :: T.EnumConstructor -> H.Definition
        ctorFun ctor =
          let 
            ctorParameterTypes = fmap transformType $ T.enumConstructorPars ctor
            ctorParameterCount = List.length ctorParameterTypes
            ctorParameterH = fmap (\(t, s) -> (("_" ++ (show s)), t)) $ zip ctorParameterTypes [0..]
            ctorName = T.enumConstructorName ctor
            ctorParameterExprs = fmap (\(s, t) -> H.Read t s) ctorParameterH 
            ctorBody = H.Immutable $ H.Array ((H.Read enumTagType $ (ctorTagName ctor)) : ctorParameterExprs)
          in 
            if ctorParameterCount == 0 then
              H.ValueDefinition ctorName enumType ctorBody
            else
              H.FunctionDefinition ctorName ctorParameterH enumType $ simpleBlock ctorBody
    
        tags = fmap ctorTag $ zip constructors [1..]
        functions = fmap ctorFun constructors
      in tags ++ functions

    T.ImplicitDef name typ expr ->
      let
        findReturnType (T.FunctionType _ a) = findReturnType a
        findReturnType a = a
    
        (transformedExpr, hiddenParams) = runState (transformExpr expr) 0
        returnType = transformType $ findReturnType typ
        functionBody = simpleBlock $ realBody transformedExpr
    
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
      let
        tupleRecField f = 
          (T.recFieldName f, transformType (T.recFieldType f))
    
        params = fmap tupleRecField fields
        returnType = H.TypeName name
        objName = "_a"
        readObj = H.Read returnType objName 
        decls = [(objName, returnType, Just $ H.Allocate returnType)]
        initStmts = fmap (\(name, t) -> H.Mutate readObj name $ H.Read t name) params
        retStmt = H.Return $ H.Read returnType objName
        body = H.Block decls (initStmts ++ [retStmt])
      in
        [
          H.FunctionDefinition name params returnType body
        ]

    T.ValDef name params typ expr ->
      let
        (transformedExpr, hiddenParams) = 
          runState (transformExpr expr) 0

        transformedParams = 
          fmap (\(T.Parameter s t) -> (s, transformType t)) params

        returnType = 
          transformType $ T.returnType typ

        functionBody = 
          H.Block [] [H.Return $ realBody transformedExpr]
    
        allParams :: [(String, H.Type)]
        allParams = 
          transformedParams ++ (bodyParams transformedExpr)
      in if (List.length allParams) == 0 then
        [
          H.ValueDefinition name (transformType typ) transformedExpr
        ]
      else 
        [
          H.FunctionDefinition name allParams returnType functionBody
        ]


{- Create a simple block that returns the expression parameter. -}
simpleBlock :: H.Expression -> H.Block
simpleBlock e =
  H.Block [] [H.Return e]
    
realBody :: H.Expression -> H.Expression
realBody (H.Lambda _ a) = realBody a
realBody a = a

bodyParams :: H.Expression -> [(String, H.Type)]
bodyParams (H.Lambda p b) = p ++ (bodyParams b)
bodyParams _ = []
  