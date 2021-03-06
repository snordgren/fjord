module Main where

import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Compiler as Compiler

main :: IO ()
main = 
  do
    args <- Env.getArgs
    let argsLength = length args
    if argsLength == 3 then
      let 
        srcDir = args !! 0
        outputDir = args !! 1
        file = args !! 2
      in
        runCompilerWith srcDir outputDir file
    else
      putStrLn "Use \"fjc <src-dir> <output-dir> <file>\" to compile a file."


runCompilerWith :: FilePath -> FilePath -> FilePath -> IO ()
runCompilerWith srcDir outDir file =
  do
    result <- Compiler.runCompiler srcDir file
    case result of 
      Left msg -> 
        emitError msg

      Right (js, typeDef) ->
        emitFiles srcDir outDir file js typeDef

emitError :: String -> IO ()
emitError msg = 
  putStrLn msg


emitFiles :: String -> String -> String -> String -> String -> IO ()
emitFiles srcDir outDir file js typeDef =
  do
    let rawFileName = FilePath.takeBaseName file
    let outJSFile = outDir ++ rawFileName ++ ".js"
    let outTypeDefFile = outDir ++ rawFileName ++ ".d.fj"
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory outJSFile
    IO.writeFile outJSFile js
    IO.writeFile outTypeDefFile typeDef
    return ()

