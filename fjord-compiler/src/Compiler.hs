module Compiler ( 
  compile,
  generateJSParameters
) where

import Data.Either.Combinators
import Data.List.NonEmpty as NonEmpty
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
  (DL.intercalate "\n\n" (fmap translateDeclaration (T.moduleDeclarations m))) ++ "\n"

generateJSParameters :: [T.Parameter] -> String
generateJSParameters parameters = 
  if DL.length parameters > 0 then 
    DL.foldr (\s -> \p -> s ++ " => " ++ p) "" (fmap T.parameterName parameters)
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
  "(" ++ (translateExpression a) ++ "(" ++ (translateExpression b) ++ ")" ++ ")"
