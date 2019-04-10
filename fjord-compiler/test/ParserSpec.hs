module ParserSpec (testParser) where 

import Data.Either.Combinators (mapLeft)
import Text.Megaparsec (errorBundlePretty, runParser, ParseErrorBundle)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)

import Parser (expressionP)
import qualified AST.Contextual as AST

testParser :: TestTree
testParser = testGroup "ParserSpec" 
  [testCase "testParseAddition" testParseAddition]

testParseAddition :: Assertion
testParseAddition = 
  let 
    result :: Either String AST.Expression
    result = mapLeft errorBundlePretty (runParser expressionP "" "1 + 2")
  in
    assertEqual "parse result" 
      (Right $ AST.Addition 1 (AST.IntLiteral 0 1) (AST.IntLiteral 4 2)) 
      result
