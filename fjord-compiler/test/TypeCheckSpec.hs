module TypeCheckSpec where 

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, testCaseSteps, Assertion)

import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified TypeCheck as TypeCheck
  
test :: TestTree
test = testGroup "TypeCheckSpec"
  [
    testCase "testFunctionParameterList" testFunctionParameterList,
    testCase "testInferRequiredBody" testInferRequiredBody
  ]

testFunctionParameterList =
  let 
    expected = [U.BuiltInInt 0, U.BuiltInInt 0]

    result = TypeCheck.functionParameterList (U.FunctionType 0 (U.BuiltInInt 0)
      (U.FunctionType 0 (U.BuiltInInt 0) (U.BuiltInInt 0)))
  in
    assertEqual "functionParameterList" expected result

testInferRequiredBody = 
  let 
    expected = U.FunctionType 0 (U.BuiltInString 0) (U.BuiltInInt 0)

    result = (TypeCheck.inferRequiredBody 
      (U.FunctionType 0 (U.BuiltInInt 0) 
        (U.FunctionType 0 (U.BuiltInString 0) (U.BuiltInInt 0))) [U.Parameter 0 "a"])
  in 
    assertEqual "inferRequiredBody" expected result
