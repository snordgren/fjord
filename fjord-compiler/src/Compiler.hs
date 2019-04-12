module Compiler ( 
  compile,
  generateJSParameters
) where

import Control.Monad.Writer.Lazy
import Data.Either.Combinators
import Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Set as Set
import Data.Void
import Text.Megaparsec
import qualified Control.Monad as Monad
import qualified Data.List as List

import Parser (runModuleParser)
import Resolution (resolve, ResolutionError (..))
import TypeCheck (typeCheck, TypeError (..))
import qualified AST.Contextual as C
import qualified AST.Typed as T

compile :: String -> String -> String
compile fileName source = 
  either errorBundlePretty generateJSModule (parseModuleSource fileName source)

parseModuleSource :: String -> String -> Either (ParseErrorBundle String String) T.Module
parseModuleSource fileName source = 
  let 
    initialPosState = PosState {
      pstateInput = source,
      pstateOffset = 0,
      pstateSourcePos = initialPos fileName,
      pstateTabWidth = defaultTabWidth,
      pstateLinePrefix = ""
    } 
  in do
    contextualModule <- runModuleParser fileName source
    resolvedModule <- mapLeft (resolvedizationErrorToErrorBundle initialPosState) 
      (resolve contextualModule)
    typedModule <- mapLeft (typeErrorToErrorBundle initialPosState) 
      (typeCheck resolvedModule)
    return typedModule
  

typeErrorToErrorBundle :: PosState String -> TypeError -> ParseErrorBundle String String
typeErrorToErrorBundle initialPosState (WrongType offset expected actual) = 
  let 
    msg = "expression has type " ++ (show actual) ++ ", expected " ++ (show expected)
  in
    toErrorBundle initialPosState offset msg

typeErrorToErrorBundle initialPosState (CannotInferType offset s) = 
  toErrorBundle initialPosState offset ("cannot infer type\n" ++ s)

typeErrorToErrorBundle initialPosState (UndefinedInScope offset) =
  toErrorBundle initialPosState offset ("undefined in scope")

toErrorBundle initialPosState offset s = 
  ParseErrorBundle {
    bundleErrors = NonEmpty.fromList [
      FancyError offset (Set.fromList [
        ErrorCustom s
      ])
    ],
    bundlePosState = initialPosState
  }

resolvedizationErrorToErrorBundle 
  :: PosState String 
  -> ResolutionError 
  -> ParseErrorBundle String String
resolvedizationErrorToErrorBundle initialPosState (TypeNotFound offset name) =
  ParseErrorBundle {
    bundleErrors = NonEmpty.fromList [
      FancyError offset (Set.fromList [
        ErrorCustom ("type not found " ++ name)
      ])
    ],
    bundlePosState = initialPosState
  }

generateJSModule :: T.Module -> String
generateJSModule m = 
  let 
    declarationOutput = 
      (List.intercalate "\n\n" 
        (List.concat (fmap translateDeclaration (T.moduleDeclarations m)))) ++ "\n"
  in 
    "// module " ++ (T.moduleName m) ++ "\n\n" ++ declarationOutput

generateJSParameters :: [String] -> String
generateJSParameters p = 
  if List.length p == 1 then
    (List.head p) ++ " => "
  else if List.length p >= 2 then 
    "(" ++ (List.intercalate ", " p) ++ ") => "
  else
    ""

translateDeclaration :: T.Declaration -> [String]

translateDeclaration (T.EnumDeclaration name constructors) = 
  let 
    translateConstructor :: (T.EnumConstructor, Int) -> [String]
    translateConstructor (a, n) = 
      [constructorTag (a, n), constructorF a]

    constructorTag :: (T.EnumConstructor, Int) -> String
    constructorTag (a, n) = 
      "export const $Tag" ++ (T.enumConstructorName a) ++ " = " ++ (show (n + 1)) ++ ";"

    constructorF :: T.EnumConstructor -> String
    constructorF a = 
      let 
        parameterCount = List.length $ functionParameterList $ T.enumConstructorType a
        parameterNames = fmap (\n -> "_" ++ (show n)) [0..(parameterCount - 1)]
        params = List.intercalate ", " parameterNames
        name = T.enumConstructorName a
        tagName = "$Tag" ++ name
        arrayParams = List.intercalate ", " (tagName : parameterNames)
      in if parameterCount == 0 then
        "export const " ++ name ++ " = Object.freeze([" ++ tagName ++ "]);" 
      else
        "export const " ++ name ++ " = (" ++ params ++ 
          ") => Object.freeze([" ++ arrayParams ++ "]);" 
  in 
    List.concat $ fmap translateConstructor (List.zip constructors [0..]) 

translateDeclaration (T.RecordDeclaration name fields) = 
  let
    jsParams = List.intercalate ", " (fmap T.recordFieldName fields)
    generateObj = "({ " ++ jsParams ++ " })";
  in
    ["export const " ++ name ++ " = (" ++ jsParams ++ ") => " ++ generateObj ++ ";"]

translateDeclaration (T.ValueDeclaration name parameters declaredType expression) =
  let
    parameterNames :: [String]
    parameterNames = fmap (T.parameterName) parameters

    (jsExpr, jsHiddenParam) = runWriter $ translateExpression expression
    jsParam = generateJSParameters (parameterNames ++ jsHiddenParam)
  in
    ["export const " ++ name ++ " = " ++ jsParam ++ jsExpr ++ ";"]

translateExpression :: T.Expression -> Writer [String] String
translateExpression (T.IntLiteral a) = 
  return $ show a

translateExpression (T.StringLiteral a) = 
  return $ show a

translateExpression (T.Name a t) = 
  return $ a

translateExpression (T.Addition a b) = do
  translatedA <- translateExpression a
  translatedB <- translateExpression b
  return ("(" ++ translatedA ++ " + " ++ translatedB ++ ")")

translateExpression (T.Apply a b) = do
  let rootF = fromMaybe a (rootFunction a)
  let requiredArgumentCount = List.length $ functionParameterList (typeOf rootF)
  let passedParameters = parametersOfApply (T.Apply a b)
  translatedParams <- Monad.sequence $ fmap translateExpression passedParameters
  translatedRootF <- translateExpression rootF
  let hiddenParamCount = requiredArgumentCount - (List.length passedParameters)
  let hiddenParams = fmap (\n -> "_" ++ (show n)) [0..(hiddenParamCount - 1)]
  let params = List.intercalate ", " (translatedParams ++ hiddenParams)
  tell hiddenParams
  return ("(" ++ translatedRootF ++ "(" ++ params ++ "))")

translateExpression (T.Lambda name t expr) =
  let 
    lambdaParameters :: T.Expression -> [String]
    lambdaParameters (T.Lambda n _ body) = n : (lambdaParameters body)
    lambdaParameters _ = []

    lambdaBody :: T.Expression -> T.Expression
    lambdaBody (T.Lambda _ _ body) = lambdaBody (body)
    lambdaBody a = a

    parameters = name : (lambdaParameters expr)
    paramsS = List.intercalate ", " parameters
  in do
    exprT <- translateExpression $ lambdaBody expr
    return $ "(" ++ paramsS ++ ")" ++ " => " ++ exprT

translateExpression (T.RecordUpdate target updates) = do
  updatesSM <- Monad.sequence $ fmap translateFieldUpdate updates
  let 
    updatesS :: String
    updatesS = List.intercalate "\n" updatesSM
  targetS <- translateExpression target
  let returnS = "\n  return _m;\n}"
  return $ "{\n  const _m = " ++ targetS ++ ";\n" ++ updatesS ++ returnS

translateFieldUpdate :: T.FieldUpdate -> Writer [String] String
translateFieldUpdate a = do 
  exprS <- translateExpression $ T.fieldUpdateExpression a
  return $ "  _m." ++ (T.fieldUpdateName a) ++ " = " ++ exprS ++ ";"

functionParameterList :: T.Type -> [T.Type]
functionParameterList (T.FunctionType a b) = [a] ++ functionParameterList b 
functionParameterList _ = []

rootFunction :: T.Expression -> Maybe T.Expression
rootFunction e = 
  case e of 
    T.Apply a b -> Just $ fromMaybe a (rootFunction a)
    _ -> Nothing

parametersOfApply :: T.Expression -> [T.Expression]
parametersOfApply e = 
  case e of 
    T.Apply a b -> (parametersOfApply a) ++ [b]
    _ -> []

typeOf :: T.Expression -> T.Type
typeOf (T.IntLiteral _) = T.BuiltInInt
typeOf (T.Name _ t) = t
