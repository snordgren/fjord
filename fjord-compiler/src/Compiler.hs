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

import Check.Types.Common (TypeError (..))
import Check.Types (typeCheck)
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
    return $ Compiler.compileM dir typeDefs path src


handleErrBundle 
  :: Either (ParseErrorBundle String String) a 
  -> Either String a
handleErrBundle e =
  Combinators.mapLeft errorBundlePretty e


compileM 
  :: String
  -> [(String, String)] 
  -> String 
  -> String 
  -> Either String (String, String)
compileM dir typeDefSources fileName fileContents =
  do
    typeDefs <- handleErrBundle $ Monad.sequence $ fmap (\(a, b) -> parseTypeDef dir a b) typeDefSources
    mod <- handleErrBundle $ parseModuleSource typeDefs fileName fileContents
    return (generateJSModule fileName mod, TypeDef.genDefStr mod)


{-|
Read the type defs in the folder.
-}
readTypeDefs :: String -> IO [(String, String)]
readTypeDefs dir =
  do
    paths <- getFilesWithin dir
    let typeDefPaths = filter (List.isSuffixOf ".d.fj") paths
    Monad.sequence $ fmap (\p -> fmap (\a -> (p, a)) $ readFile p) typeDefPaths


getFilesWithin :: String -> IO [String]
getFilesWithin dir = 
  do
    isDir <- Directory.doesDirectoryExist dir
    if isDir 
      then
        do
          paths <- Directory.listDirectory dir
          within <- Monad.sequence $ fmap (\s -> getFilesWithin (dir ++ "/" ++ s)) paths
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
    Combinators.mapLeft (typeErrorToErrorBundle $ genPosState fileName fileContents) 
      (typeCheck typeDefs untypedMod)
  

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

      ExpectedUnique offset -> 
        bundleMsg offset $ "expected unique value"

      CannotInferType offset s -> 
        bundleMsg offset ("cannot infer type\n" ++ s)

      ImplicitNotFound offset typ name ->
        bundleMsg offset ("cannot find implicit " ++ name ++ " of type " ++ show typ)

      ImportNotFound (U.Import offset name) -> 
        bundleMsg offset ("cannot find import " ++ name)

      TooFewUsages offset name -> 
        bundleMsg offset ("too few usages of " ++ name)

      TooManyParameters offset expectedCount -> 
        bundleMsg offset ("too many parameters, expected " ++ (show expectedCount))

      TooManyUsages offset name -> 
        bundleMsg offset ("too many usages of " ++ name)

      UndefinedInScope offset ->
        bundleMsg offset ("undefined in scope")

      UnknownType offset s -> 
        bundleMsg offset $ "unknown type " ++ s


generateJSModule :: String -> T.Module -> String
generateJSModule srcPath m = JS.generateJS srcPath $ ToHybrid.transformModule m
