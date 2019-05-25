# Fjord

Fjord is a strict programming language that compiles to JavaScript.

## Features
 * [x] JavaScript interop via `.d.fj`-definition files, a human-readable 
       format that uses the same type notation as Fjord to describe the types of 
       JavaScript dependencies. 
 * [x] Enum types, also known as algebraic data types or (disjoint) union types. 
       Like Scala 3 and Rust, Fjord uses the `enum` keyword to declare a type 
       with multiple data constructors. 
   * [x] Pattern matching on enum types. 
 * [x] Custom operators with precedence based on the first token, like Scala.
 * [ ] Primitive value types
   * [x] Integer type and integer literals
   * [x] String type and string literals
   * [ ] Floating-point value literals. 
   * [ ] Array type and array literals.
   * [ ] Escaped characters in string literals.
 * [ ] Modules.
   * [x] Importing all members of another module into the global scope.
   * [ ] Selectively importing functions, operators, and types from other modules.
   * [ ] Aliasing imported functions, operators, and types.
   * [ ] Aliasing imported modules.
 * [ ] Tuples.
   * [x] Type-system support.
   * [x] Tuple value construction.
   * [ ] Destructuring in let bindings.
 * [ ] Implicit values.
   * [x] Resolution of concrete and polymorphic types.
   * [ ] Higher-order implicit value construction.
 * [ ] Records, also known as structs, containing named data fields. 
   * [x] Shared record field access.
   * [ ] Record destructuring in let bindings.
   * [ ] Type and lifetime-safe record mutation.
 * [ ] *Typed holes*, `?`-prefixed identifiers where the compiler tells you what 
       type of value it is missing for that identifier. 
 * [ ] Lifetime checking for sharing unique values safely, especially in partially 
       applied or curried functions.
 * [ ] Compiler optimizations for common patterns in functions and data structures. 
   * [ ] Inline functions when deemed appropriate.
