# fjord-compiler

## Libraries
The compiler uses Megaparsec, which is unfortunate given the dearth of 
resources for it, but its very similar to Parsec. 

## Workflow
The Fjord compiler primarily uses *golden testing*, where test input and 
expected output are stored as files.

**To add a new feature or change an existing one**, add or change a file in the 
appropriate folder under `test`. Use `codegen` for expected generated output and 
`errors` for expected errors. The `.fj` file is the input to the compiler, and 
the `.js|.golden` file is the expected output from the compiler. 

## Testing
Use `stack test` to run the test suite. 
