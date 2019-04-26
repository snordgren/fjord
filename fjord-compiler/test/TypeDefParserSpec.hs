module TypeDefParserSpec where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, Assertion)

import Parser.TypeDef.Parser (typeDefP)
import ParserSpecUtil (runParserTest)
import qualified AST.Untyped as U


testParser :: TestTree
testParser = testGroup "TypeDefParserSpec" 
  [
    testCase "testParseTypeDef" testParseTypeDef
  ]

testParseTypeDef :: Assertion
testParseTypeDef = 
  let 
    expected = U.TypeDef "a" 
      [
        U.DeclRecDecl $ U.RecDecl 9 "OneMember" 
          [
            U.RecField 28 "funds" $ U.FunctionType 45 (U.TypeName 36 "OneMember") $ 
              U.TypeName 49 "Int"
          ],
        U.DeclRecDecl $ U.RecDecl 54 "TwoMembers"
          [
            U.RecField 74 "fn" $ U.FunctionType 82 (U.TypeName 79 "Int") $ 
              U.TypeName 86 "OneMember",
            U.RecField 98 "count" $ U.TypeName 106 "Int"
          ],
        U.DeclEnumDecl $ U.EnumDecl 111 "ThreeMembers" 
          [
            U.EnumConstructor 131 "First" $ U.TypeName 139 "ThreeMembers",
            U.EnumConstructor 154 "Second" $ U.FunctionType 166 (U.TypeName 163 "Int")
              (U.TypeName 170 "ThreeMembers"),
            U.EnumConstructor 185 "Third" $ U.FunctionType 220 
              (U.TupleType 193 
                [
                  U.FunctionType 204 (U.TypeName 194 "TwoMembers") (U.TypeName 208 "String"),
                  U.TypeName 216 "Int"
                ]
              )
              (U.TypeName 224 "ThreeMembers")
          ],
        U.DeclValDecl (U.ValDecl 238 "five" [] $ U.TypeName 245 "Int"),
        U.DeclValDecl (U.ValDecl 250 "stringIdentity" [] $ 
          U.FunctionType 273 (U.TypeName 267 "String") (U.TypeName 277 "String")),
        U.DeclValDecl $ U.ValDecl 285 "fn0" [] $ 
          U.FunctionType 309 
            (U.FunctionType 298 (U.TypeName 292 "String") $ U.TypeName 302 "String")
            (U.TypeName 313 "String")
      ]

    source = 
      "module a\n" ++
      "record OneMember\n" ++
      "  funds : OneMember -> Int\n\n" ++
      "record TwoMembers\n" ++
      "  fn : Int -> OneMember\n" ++
      "  count : Int\n\n" ++
      "enum ThreeMembers\n" ++
      "  First : ThreeMembers\n" ++
      "  Second : Int -> ThreeMembers\n" ++
      "  Third : (TwoMembers -> String, Int) -> ThreeMembers\n\n" ++
      "five : Int\n\n" ++
      "stringIdentity : String -> String\n\n" ++
      "fn0 : (String -> String) -> String\n"
  in
    runParserTest typeDefP expected source
      
