module Compiler ( 
  compile
) where

import Text.Megaparsec
import qualified Data.Either.Combinators as Combinators
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import Resolution (resolve, ResolutionError (..))
import TypeCheck (typeCheck, TypeError (..))
import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified CodeGen.JS as JS
import qualified CodeGen.TypeDef as TypeDef
import qualified Parser.Source.Parser as Source.Parser
import qualified Transform.ToHybrid as ToHybrid

compile :: String -> String -> (String, String)
compile fileName source = 
  let 
    handleErr bundle = (errorBundlePretty bundle, "")
    handleSucc mod = (generateJSModule mod, TypeDef.genDefStr mod)
  in 
    either handleErr handleSucc $ parseModuleSource fileName source

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
    contextualModule <- Source.Parser.runModuleParser fileName source
    resolvedModule <- Combinators.mapLeft (resolvedizationErrorToErrorBundle initialPosState) 
      (resolve contextualModule)
    typedModule <- Combinators.mapLeft (typeErrorToErrorBundle initialPosState) 
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
generateJSModule m = JS.generateJS $ ToHybrid.transformModule m
