module Compiler ( 
  readTypeDefs,
  runCompiler
) where

import Debug.Trace
import Text.Megaparsec
import qualified Control.Monad as Monad
import qualified Data.Either.Combinators as Combinators
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified System.Directory as Directory

import Check.Types.Common
import Check.Types (typeCheck)
import Transform.Resolve (resolveModule)
import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified CodeGen.JS as JS
import qualified CodeGen.TypeDef as TypeDef
import qualified Parser.Source.Parser as Source.Parser
import qualified Parser.TypeDef.Parser as TypeDef.Parser
import qualified Transform.ToHybrid as ToHybrid

runCompiler :: FilePath -> FilePath -> IO (Either String (String, String))
runCompiler dir path =
  do
    typeDefs <- Compiler.readTypeDefs dir
    src <- readFile path
    let compileResult = Compiler.compileM dir typeDefs path src
    return compileResult


parseModule 
  :: String 
  -> [(String, String)] 
  -> String 
  -> String 
  -> Either (ParseErrorBundle String String) T.Module
parseModule dir typeDefSources fileName fileContents =
  do
    typeDefs <- traverse (\(a, b) -> parseTypeDef dir a b) typeDefSources
    parseModuleSource typeDefs fileName fileContents


compileM 
  :: String
  -> [(String, String)] 
  -> String 
  -> String 
  -> Either String (String, String)
compileM dir typeDefSources fileName fileContents =
  do
    let parseRes = parseModule dir typeDefSources fileName fileContents
    parseRes0 <- Combinators.mapLeft errorBundlePretty parseRes
    resolvedModule <- resolveModule parseRes0
    return (generateJSModule fileName resolvedModule, TypeDef.genDefStr resolvedModule)

{-|
Read the type defs in the folder.
-}
readTypeDefs :: String -> IO [(String, String)]
readTypeDefs dir =
  do
    paths <- getFilesWithin dir
    let typeDefPaths = filter (List.isSuffixOf ".d.fj") paths
    traverse (\p -> fmap (\a -> (p, a)) $ readFile p) typeDefPaths


getFilesWithin :: String -> IO [String]
getFilesWithin dir = 
  do
    isDir <- Directory.doesDirectoryExist dir
    if isDir 
      then
        do
          paths <- Directory.listDirectory dir
          within <- traverse (\s -> getFilesWithin (dir ++ "/" ++ s)) paths
          let addedPaths = fmap (\a -> dir ++ "/" ++ a) paths
          return (addedPaths ++ (List.concat within))
      else
        return []


genPosState :: String -> String -> PosState String
genPosState fileName fileContents = 
  PosState {
    pstateInput = fileContents,
    pstateOffset = 0,
    pstateSourcePos = initialPos fileName,
    pstateTabWidth = defaultTabWidth,
    pstateLinePrefix = ""
  }


parseTypeDef :: String -> String -> String -> Either (ParseErrorBundle String String) U.TypeDef
parseTypeDef dir fileName fileContents = 
  runParser (TypeDef.Parser.typeDefP fileName) fileName fileContents
    

parseModuleSource 
  :: [U.TypeDef]
  -> String 
  -> String 
  -> Either (ParseErrorBundle String String) T.Module
parseModuleSource typeDefs fileName fileContents = 
  do
    untypedMod <- Source.Parser.runModuleParser fileName fileContents
    Combinators.mapLeft (makeErrorBundlePretty $ genPosState fileName fileContents) 
      (typeCheck typeDefs untypedMod)
  

makeErrorBundlePretty :: PosState String -> TypeErrorAt -> ParseErrorBundle String String
makeErrorBundlePretty initialPosState (offset, err) =
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

    msg = err {-
      case err of 
        WrongType expected actual -> 
          "expression has type " ++ (show actual) ++ ", expected " ++ (show expected)

        ExpectedNonUnique -> 
          
    
        ExpectedUnique -> 
           "expected unique value"
  
        ImplicitNotFound typ name ->
          
    
        ImportNotFound (U.Import _ name) -> 
          ("cannot find import " ++ name)
                          -}
  in
    bundleMsg offset msg


generateJSModule :: String -> T.Module -> String
generateJSModule srcPath m = JS.generateJS srcPath $ ToHybrid.transformModule m
