import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Compiler as Compiler
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
    ParserSpec.testParser,
    TypeCheckSpec.test
  ]

goldenTests :: IO TestTree
goldenTests = do
  js <- jsGoldenTests
  errors <- errorGoldenTests
  typeDef <- typeDefGoldenTests
  return $ testGroup "Golden Tests" [js, errors, typeDef]

jsGoldenTests :: IO TestTree
jsGoldenTests = createGoldenTestTree "JS CodeGen" "test/codegen" ".js" fst

errorGoldenTests :: IO TestTree
errorGoldenTests = createGoldenTestTree "Error Reporting" "test/errors" 
  ".golden" fst

typeDefGoldenTests :: IO TestTree
typeDefGoldenTests = createGoldenTestTree "Definition file generation" "test/typedef" 
  ".d.fj" (\(a, b) -> if length b > 0 then b else a)

createGoldenTestTree name dir extension f = do
  files <- findByExtension [".fj"] dir
  tests <- mapM (mkGoldenTest extension f) $ 
    filter (\a -> not $ List.isInfixOf ".d.fj" $ FilePath.takeFileName a) files
  return $ testGroup name tests

mkGoldenTest :: String -> ((String, String) -> String) -> FilePath -> IO TestTree
mkGoldenTest extension f path = do  
  let testName = FilePath.takeBaseName path
  let goldenPath = FilePath.replaceExtension path extension
  return $ goldenVsString testName goldenPath action
  where
    action :: IO LBS.ByteString
    action = 
      let 
        dir = "test/"
      in
        do
          res <- Compiler.runCompiler dir path
          return $ LBS.pack $ f res
