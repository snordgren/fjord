import System.FilePath (takeBaseName, takeFileName, replaceExtension)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Compiler as  C
import qualified AST.Typed as T
import qualified ParserSpec as ParserSpec
import qualified TypeCheckSpec as TypeCheckSpec

main :: IO ()
main = do
  foundGoldenTests <- goldenTests
  defaultMain $ testGroup "Tests" [unitTests, foundGoldenTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" 
  [
    testGroup "Compiler" [
      testCase "generateJSParameters" testGenerateJSParameters
    ],
    ParserSpec.testParser,
    TypeCheckSpec.test
  ]

testGenerateJSParameters :: Assertion
testGenerateJSParameters = 
  assertEqual "generated parameter list" "(x, y) => " 
    (C.generateJSParameters ["x", "y"])

goldenTests :: IO TestTree
goldenTests = do
  js <- jsGoldenTests
  errors <- errorGoldenTests
  return $ testGroup "Golden Tests" [js, errors]

jsGoldenTests :: IO TestTree
jsGoldenTests = createGoldenTestTree "JS CodeGen" "./test/codegen" ".js"

errorGoldenTests :: IO TestTree
errorGoldenTests = createGoldenTestTree "Error Reporting" "./test/errors" 
  ".golden"

createGoldenTestTree name dir extension = do
  files <- findByExtension [".fj"] dir
  tests <- mapM (mkGoldenTest extension) files
  return $ testGroup name tests

mkGoldenTest :: String -> FilePath -> IO TestTree
mkGoldenTest extension path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path extension
  return (goldenVsString testName goldenPath action)
  where
    action :: IO LBS.ByteString
    action = do
      source <- readFile path
      let actual = C.compile (takeFileName path) source
      return (LBS.pack actual)
