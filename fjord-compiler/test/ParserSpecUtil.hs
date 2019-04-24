{-# LANGUAGE RankNTypes #-}
module ParserSpecUtil (
  runParserTest
) where
  
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)
import Text.Megaparsec
import qualified Data.Either.Combinators as Combinators

import Parser.Common (Parser)

runParserTest :: forall a. (Eq a, Show a) => Parser a -> a -> String -> Assertion
runParserTest parser expected src = 
  let 
    result = Combinators.mapLeft errorBundlePretty (runParser parser "" src)
  in
    assertEqual "parse result" (Right expected) result