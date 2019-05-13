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
    (js, typeDef) <- Compiler.runCompiler srcDir file
    let outJSFile = outDir ++ (FilePath.replaceExtension (drop (length srcDir) file) ".js")
    let outTypeDefFile = outDir ++ (FilePath.replaceExtension (drop (length srcDir) file) ".d.fj")
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory outJSFile
    IO.writeFile outJSFile js
    IO.writeFile outTypeDefFile typeDef
    return ()
