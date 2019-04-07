import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath (takeBaseName, takeFileName, replaceExtension)

import Compiler (compile)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, findByExtension)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  files <- findByExtension [".fj"] "./test/backend/js"
  tests <- mapM mkGoldenTest files
  return $ testGroup "JS Target golden tests" tests

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  return (goldenVsString testName goldenPath action)
  where
    action :: IO LBS.ByteString
    action = do
      source <- readFile path
      let actual = compile (takeFileName path) source
      return (LBS.pack actual)
