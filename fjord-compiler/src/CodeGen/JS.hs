module CodeGen.JS where

import Debug.Trace
import qualified Data.List as List
import qualified Data.List.Utils as List.Utils
import qualified Data.Maybe as Maybe

import qualified AST.Hybrid as H
import qualified CodeGen.NameMangling as NameMangling
import qualified Utils as Utils

generateJS :: String -> H.Source -> String
generateJS srcPath s = 
  let 
    moduleS = "// module " ++ (H.sourceName s) ++ "\n\n"
    importsS = concatMap (depToJS srcPath) $ H.sourceDeps s
    definitionS = List.intercalate "\n\n" $ fmap definitionToJS $ H.sourceDefinitions s
  in 
    moduleS ++ importsS ++ (if (List.length importsS > 0) then "\n" else "") ++ definitionS ++ "\n"    


depToJS :: String -> H.Dependency -> String
depToJS srcPath dep =
  let 
    alias = NameMangling.mangleImport $ H.dependencyAlias dep

    srcHead = 
      List.concat $ fmap (\_ -> "../") $ filter (\a -> a == '/') srcPath      

    src = 
      List.Utils.replace ".d.fj" ".js" $ H.dependencySource dep

    reqPath =
      Utils.replaceRec "//" "/" (srcHead ++ src)
  in
    "var " ++ alias ++ " = require(\"" ++ reqPath ++ "\");\n"


definitionToJS :: H.Definition -> String

definitionToJS (H.FunctionDefinition name parameters returnType body) = 
  let
    params = List.intercalate ", " $ fmap fst parameters
    parameterCount = List.length parameters
    paramsJS = case parameterCount of 
      0 -> "()"
      _ -> "(" ++ params ++ ")"

    bodyJS = "{\n" ++ (blockToJS 1 body) ++ "\n}"

    mangledName = NameMangling.mangle name
  in 
    "var " ++ mangledName ++ " = function" ++ paramsJS ++ " " ++ bodyJS ++ ";\n" ++
    "exports." ++ mangledName ++ " = " ++ mangledName ++ ";"

definitionToJS (H.ValueDefinition name typ expr) = 
  let 
    mangledName = NameMangling.mangle name
    exprJS = expressionToJS 0 expr
  in
    "var " ++ mangledName ++ " = " ++ exprJS ++ ";\n" ++
    "exports." ++ mangledName ++ " = " ++ mangledName ++ ";"


expressionToJS :: Int -> H.Expression -> String
expressionToJS indent (H.Addition _ a b) = 
  "(" ++ (expressionToJS indent a) ++ " + " ++ (expressionToJS indent b) ++ ")"

expressionToJS _ (H.Allocate _) =
  "{}"

expressionToJS indent (H.Array exprs) = 
  "[" ++ (List.intercalate ", " $ fmap (expressionToJS indent) exprs) ++ "]"

expressionToJS indent (H.ArrayAccess arrayExpression indexExpression) = 
  let 
    arrayJS = expressionToJS indent arrayExpression
    indexJS = expressionToJS indent indexExpression
  in
    "(" ++ arrayJS ++ ")[" ++ indexJS ++ "]"

expressionToJS indent (H.Equals a b) = 
  "(" ++ (expressionToJS indent a) ++ " === " ++ (expressionToJS indent b) ++ ")"

expressionToJS indent (H.FieldAccess fieldName target) = 
  expressionToJS indent target ++ "." ++ fieldName

expressionToJS indent (H.IIFE b) = 
  "(function() {\n" ++ (blockToJS (indent + 1) b) ++ "\n" ++ (indentF indent) ++ "})()"

expressionToJS indent (H.Immutable expr) = 
  "Object.freeze(" ++ (expressionToJS indent expr) ++ ")"

expressionToJS _ (H.IntLiteral n) = 
  show n

expressionToJS indent (H.Invoke f params) = 
  let 
    paramsJS = List.intercalate ", " $ fmap (expressionToJS indent) params
  in
    "(" ++ (expressionToJS indent f) ++ "(" ++ paramsJS ++ "))"

expressionToJS indent (H.Lambda variables body) = 
  let 
    exprJS = 
      expressionToJS (indent + 1) body

    bodyJS = 
      (indentF (indent + 1)) ++ "return " ++ exprJS ++ ";\n"

    paramJS = 
      "(" ++ (List.intercalate ", " $ fmap fst variables) ++ ")"

    indentS = 
      indentF indent
  in
    "function" ++ paramJS ++ " {\n" ++ bodyJS ++ indentS ++ "}"

expressionToJS _ (H.Read _ n) = 
  n

expressionToJS _ (H.ReadImport _ n imp) =
  (NameMangling.mangleImport imp) ++ "." ++ n

expressionToJS _ (H.StringLiteral s) = 
  show s


statementToJS :: Int -> H.Statement -> String
statementToJS indent (H.Assign variable expr) = 
  variable ++ " = " ++ (expressionToJS indent expr) ++ ";"

statementToJS indent (H.If thenBranches elseBranch) =
  let 
    indentS = indentF indent
    indentPlus = indentF (indent + 1)
    endBrace = "\n" ++ indentS ++ "}"

    generateIf :: (H.Expression, H.Block) -> String
    generateIf (condition, thenBlock) = 
      let 
        expressionJS = expressionToJS indent condition
        bodyJS = blockToJS (indent + 1) thenBlock
      in
        "if (" ++ expressionJS ++ ") {\n" ++ bodyJS ++ endBrace

    thenBranchesJS = 
      List.intercalate " else " $ fmap generateIf thenBranches 

    elseBranchJS = 
      Maybe.fromMaybe "" $ 
        fmap (\a -> " else {\n" ++ (blockToJS (indent + 1) a) ++ endBrace) elseBranch 
  in
    thenBranchesJS ++ elseBranchJS

statementToJS indent (H.Mutate target field expr) = 
  (expressionToJS indent target) ++ "." ++ field ++ " = " ++ (expressionToJS indent expr) ++ ";"

statementToJS indent (H.Return expr) = 
  "return " ++ (expressionToJS indent expr) ++ ";"


blockToJS :: Int -> H.Block -> String
blockToJS indent b = 
  let
    indentS = indentF indent

    declEToJS :: Maybe H.Expression -> String
    declEToJS e = Maybe.fromMaybe "" $ fmap (\a -> " = " ++ (expressionToJS indent a)) e

    declToJS :: (String, H.Type, Maybe H.Expression) -> String
    declToJS (s, _, e) = indentS ++ "var " ++ s ++ (declEToJS e) ++ ";"
    
    declJS = fmap declToJS (H.blockDeclarations b)
    stmtJS = fmap (\a -> indentS ++ (statementToJS indent a)) (H.blockStatements b)
    blockJS = List.intercalate "\n" (declJS ++ stmtJS)
  in 
    blockJS


indentF n = List.concat $ fmap (const "  ") [0..(n - 1)]
