{-# LANGUAGE RankNTypes #-}
module ParserSpec (
  testParser
) where 

import Data.Either.Combinators (mapLeft)
import Text.Megaparsec (errorBundlePretty, runParser, ParseErrorBundle)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)

import Parser.Common (Parser)
import Parser.Source.Expression (caseP, expressionP, patternP)
import Parser.Source.Parser
import ParserSpecUtil (runParserTest)
import qualified AST.Untyped as AST

testParser :: TestTree
testParser = testGroup "ParserSpec" 
  [
    testCase "testParseCase" testParseCase,
    testCase "testParsePattern" testParsePattern
  ]


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
    runParserTest caseP expected "case 0 of \n  Aaaa -> 0\n  Bbbb cccc -> 1\n\n"


testParsePattern :: Assertion
testParsePattern = 
  let
    expected :: AST.Pattern 
    expected = AST.Pattern 5 "A" ["b", "c"] (AST.IntLiteral 14 1)
  in
    runParserTest patternP expected "  \n  A b c -> 1\n"
