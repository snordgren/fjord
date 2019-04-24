import System.FilePath (takeBaseName, takeFileName, replaceExtension)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List

import qualified Compiler as  C
import qualified AST.Typed as T
import qualified ParserSpec as ParserSpec
import qualified TypeCheckSpec as TypeCheckSpec
import qualified TypeDefParserSpec as TypeDefParserSpec

main :: IO ()
main = do
  foundGoldenTests <- goldenTests
  defaultMain $ testGroup "Tests" [unitTests, foundGoldenTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" 
  [
    ParserSpec.testParser,
    TypeCheckSpec.test,
    TypeDefParserSpec.testParser
  ]

goldenTests :: IO TestTree
goldenTests = do
  js <- jsGoldenTests
  errors <- errorGoldenTests
  typeDef <- typeDefGoldenTests
  return $ testGroup "Golden Tests" [js, errors, typeDef]

jsGoldenTests :: IO TestTree
jsGoldenTests = createGoldenTestTree "JS CodeGen" "./test/codegen" ".js" fst

errorGoldenTests :: IO TestTree
errorGoldenTests = createGoldenTestTree "Error Reporting" "./test/errors" 
  ".golden" fst

typeDefGoldenTests :: IO TestTree
typeDefGoldenTests = createGoldenTestTree "Definition file generation" "./test/typedef" 
  ".d.fj" (\(a, b) -> if length b > 0 then b else a)

createGoldenTestTree name dir extension f = do
  files <- findByExtension [".fj"] dir
  tests <- mapM (mkGoldenTest extension f) $ 
    filter (\a -> not $ List.isInfixOf ".d.fj" $ takeFileName a) files
  return $ testGroup name tests

mkGoldenTest :: String -> ((String, String) -> String) -> FilePath -> IO TestTree
mkGoldenTest extension f path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path extension
  return $ goldenVsString testName goldenPath action
  where
    action :: IO LBS.ByteString
    action = do
      src <- readFile path
      let res = C.compile (takeFileName path) src
      return $ LBS.pack $ f res
