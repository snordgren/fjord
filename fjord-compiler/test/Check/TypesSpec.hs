module Check.TypesSpec where 

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, testCaseSteps, Assertion)

import qualified AST.Typed as T
import qualified AST.Untyped as U
import qualified Check.Types as TypeCheck
  
test :: TestTree
test = testGroup "TypeCheckSpec"
  [
    testCase "testFnParamList" testFnParamList
  ]

testFnParamList =
  let 
    expected = [U.TypeName 0 "Int", U.TypeName 0 "Int"]

    result = TypeCheck.fnParamList (U.FunctionType 0 (U.TypeName 0 "Int")
      (U.FunctionType 0 (U.TypeName 0 "Int") (U.TypeName 0 "Int")))
  in
    assertEqual "fnParamList" expected result
