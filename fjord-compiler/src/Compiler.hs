module Compiler ( 
  compile
) where

import Text.Megaparsec
import qualified Data.Either.Combinators as Combinators
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

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
    untypedMod <- Source.Parser.runModuleParser fileName source
    typedMod <- Combinators.mapLeft (typeErrorToErrorBundle initialPosState) 
      (typeCheck untypedMod)
    return typedMod
  

typeErrorToErrorBundle :: PosState String -> TypeError -> ParseErrorBundle String String
typeErrorToErrorBundle initialPosState err =
  let 
    bundleMsg :: Int -> String -> ParseErrorBundle String String
    bundleMsg offset msg =
      ParseErrorBundle {
        bundleErrors = NonEmpty.fromList [
          FancyError offset (Set.fromList [
            ErrorCustom msg
          ])
        ],
        bundlePosState = initialPosState
      }
  in
    case err of 
      WrongType offset expected actual -> 
        let 
          msg = "expression has type " ++ (show actual) ++ ", expected " ++ (show expected)
        in
          bundleMsg offset msg

      CannotInferType offset s -> 
        bundleMsg offset ("cannot infer type\n" ++ s)

      ImplicitNotFound offset typ name ->
        bundleMsg offset ("cannot find implicit " ++ name ++ " of type " ++ show typ)

      UndefinedInScope offset ->
        bundleMsg offset ("undefined in scope")

      UndefinedType offset s -> 
        bundleMsg offset $ "unknown type " ++ s


generateJSModule :: T.Module -> String
generateJSModule m = JS.generateJS $ ToHybrid.transformModule m
