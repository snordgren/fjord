module TypeCheckSpec where 

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, testCaseSteps, Assertion)

import qualified AST.Canonical as C
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
    expected = [C.BuiltInInt 0, C.BuiltInInt 0]

    result = TypeCheck.functionParameterList (C.FunctionType 0 (C.BuiltInInt 0)
      (C.FunctionType 0 (C.BuiltInInt 0) (C.BuiltInInt 0)))
  in
    assertEqual "functionParameterList" expected result

testInferRequiredBody = 
  let 
    expected = C.FunctionType 0 (C.BuiltInString 0) (C.BuiltInInt 0)

    result = (TypeCheck.inferRequiredBody 
      (C.FunctionType 0 (C.BuiltInInt 0) 
        (C.FunctionType 0 (C.BuiltInString 0) (C.BuiltInInt 0))) [C.Parameter 0 "a"])
  in 
    assertEqual "inferRequiredBody" expected result
