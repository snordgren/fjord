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
typeErrorToErrorBundle initialPosState (WrongType offset expected actual) = ParseErrorBundle {
  bundleErrors = NonEmpty.fromList [
    FancyError offset (Set.fromList [
      ErrorCustom ("expression has type " ++ (show actual) ++ ", expected " ++ (show expected))
    ])
  ],
  bundlePosState = initialPosState
}
typeErrorToErrorBundle initialPosState (CannotInferType offset) = ParseErrorBundle {
  bundleErrors = NonEmpty.fromList [
    FancyError offset (Set.fromList [
      ErrorCustom ("cannot infer type")
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
