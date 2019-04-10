module Compiler ( 
  compile,
  generateJSParameters
) where

import Data.Either.Combinators
import Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Set as Set
import Data.Void
import Text.Megaparsec
import qualified Data.List as DL

import Canonicalize (canonicalize, CanonicalizationError (..))
import Parser (runModuleParser)
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
    canonicalModule <- mapLeft (canonicalizationErrorToErrorBundle initialPosState) 
      (canonicalize contextualModule)
    typedModule <- mapLeft (typeErrorToErrorBundle initialPosState) 
      (typeCheck canonicalModule)
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

canonicalizationErrorToErrorBundle :: PosState String -> 
  CanonicalizationError -> 
  ParseErrorBundle String String
canonicalizationErrorToErrorBundle initialPosState (TypeNotFound offset name) =
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
      (DL.intercalate "\n\n" (fmap translateDeclaration (T.moduleDeclarations m))) ++ "\n"
  in 
    "// module " ++ (T.moduleName m) ++ "\n\n" ++ declarationOutput

generateJSParameters :: [T.Parameter] -> String
generateJSParameters parameters = 
  let 
    parameterNames = (fmap T.parameterName parameters)
  in if DL.length parameters == 1 then
    (T.parameterName $ DL.head parameters) ++ " => "
  else if DL.length parameters >= 2 then 
    "(" ++ (DL.foldl' (\s -> \p -> s ++ ", " ++ p) (DL.head parameterNames) (DL.tail parameterNames)) ++ ") => "
  else
    ""

translateDeclaration :: T.Declaration -> String
translateDeclaration (T.ValueDeclaration name parameters declaredType expression) =
  let
    jsParam = generateJSParameters parameters
    jsExpr = translateExpression expression
  in
    "export const " ++ name ++ " = " ++ jsParam ++ jsExpr ++ ";"

translateExpression :: T.Expression -> String
translateExpression (T.IntLiteral a) = show a
translateExpression (T.StringLiteral a) = show a
translateExpression (T.Name a) = a
translateExpression (T.Addition a b) = 
  "(" ++ (translateExpression a) ++ " + " ++ (translateExpression b) ++ ")"
translateExpression (T.Apply a b) = 
  let 
    translatedParams = fmap translateExpression (parametersOfApply (T.Apply a b))
    params = 
      if DL.null translatedParams then
        ""
      else if DL.length translatedParams == 1 then
        DL.head translatedParams
      else
        DL.foldl' (\a -> \b -> a ++ ", " ++ b) (DL.head translatedParams) (DL.tail translatedParams)
  in 
    "(" ++ (translateExpression (fromMaybe a (rootFunction a))) ++ "(" ++ params ++ "))"

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
