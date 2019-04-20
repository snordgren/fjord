{-# LANGUAGE RankNTypes #-}
module ParserSpec (
  testParser
) where 

import Data.Either.Combinators (mapLeft)
import Text.Megaparsec (errorBundlePretty, runParser, ParseErrorBundle)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)

import Parser
import Parsing.Basics (Parser)
import Parsing.Expression (caseP, expressionP, patternP)
import qualified AST.Untyped as AST

testParser :: TestTree
testParser = testGroup "ParserSpec" 
  [
    testCase "testParseAddition" testParseAddition,
    testCase "testParseCase" testParseCase,
    testCase "testParsePattern" testParsePattern
  ]

testParseAddition :: Assertion
testParseAddition = 
  let 
    expected = AST.Operator 1 "+" (AST.IntLiteral 0 1) (AST.IntLiteral 4 2)
  in
    runTest expressionP expected "1 + 2"


testParseCase :: Assertion
testParseCase =
  let 
    expected :: AST.Expression
    expected = AST.Case 0 (AST.IntLiteral 5 0) 
      [
        AST.Pattern 13 "Aaaa" [] (AST.IntLiteral 21 0),
        AST.Pattern 25 "Bbbb" ["cccc"] (AST.IntLiteral 38 1)
      ]
  in
    runTest caseP expected "case 0 of \n  Aaaa -> 0\n  Bbbb cccc -> 1\n\n"


testParsePattern :: Assertion
testParsePattern = 
  let
    expected :: AST.Pattern 
    expected = AST.Pattern 5 "A" ["b", "c"] (AST.IntLiteral 14 1)
  in
    runTest patternP expected "  \n  A b c -> 1\n"


runTest :: forall a. (Eq a, Show a) => Parser a -> a -> String -> Assertion
runTest parser expected src = 
  let 
    result = mapLeft errorBundlePretty (runParser parser "" src)
  in
    assertEqual "parse result" (Right expected) result
