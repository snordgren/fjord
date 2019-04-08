import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath (takeBaseName, takeFileName, replaceExtension)

import Compiler (compile)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, findByExtension)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  js <- jsGoldenTests
  errors <- errorGoldenTests
  return $ testGroup "Golden Tests" [js, errors]

jsGoldenTests :: IO TestTree
jsGoldenTests = createGoldenTestTree "JS CodeGen" "./test/codegen/js" ".js"

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
      let actual = compile (takeFileName path) source
      return (LBS.pack actual)
