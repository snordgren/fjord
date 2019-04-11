module TypeCheckSpec where 

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, testCaseSteps, Assertion)

import qualified AST.Resolved as R
import qualified AST.Typed as T
import qualified TypeCheck as TypeCheck
  
test :: TestTree
test = testGroup "TypeCheckSpec"
  [
    testCase "testFunctionParameterList" testFunctionParameterList,
    testCase "testInferRequiredBody" testInferRequiredBody
  ]

testFunctionParameterList =
  let 
    expected = [R.BuiltInInt 0, R.BuiltInInt 0]

    result = TypeCheck.functionParameterList (R.FunctionType 0 (R.BuiltInInt 0)
      (R.FunctionType 0 (R.BuiltInInt 0) (R.BuiltInInt 0)))
  in
    assertEqual "functionParameterList" expected result

testInferRequiredBody = 
  let 
    expected = R.FunctionType 0 (R.BuiltInString 0) (R.BuiltInInt 0)

    result = (TypeCheck.inferRequiredBody 
      (R.FunctionType 0 (R.BuiltInInt 0) 
        (R.FunctionType 0 (R.BuiltInString 0) (R.BuiltInInt 0))) [R.Parameter 0 "a"])
  in 
    assertEqual "inferRequiredBody" expected result
