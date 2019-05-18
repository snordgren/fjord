# Fjord

Fjord is a strict programming language that compiles to JavaScript.

## Features

 
 * [x] Unique and shared values, allowing for mutation in pure code. 
 * [x] JavaScript interop via `.d.fj`-definition files, a human-readable 
       format using the same type notation as Fjord to specify the types
       of values.
 * [x] Enum types, also known as algebraic data types or (disjoint) union types. 
       Like Scala 3 and Rust, Fjord uses the `enum` keyword to declare a type 
       with multiple data constructors. 
 * [x] Pattern matching on enum types. 
 * [x] Custom operators with precedence based on the first token, like Scala.
 * [x] Integer and string literals.
 * [x] Importing other modules.
 * [ ] Principled implicit values for ad-hoc polymorphism. 
 * [ ] Generic functions, also known as parametric polymorphism.
 * [ ] Records, also known as structs, containing named data fields. 
 * [ ] Array type and array literals.
 * [ ] 64-bit floating point type and literals.
 * [ ] "Typed holes"; `?`-prefixed identifiers where the compiler tells you what 
       type of value it is missing for that identifier. 
 * [ ] Lifetime checking for sharing unique values safely, especially in partially 
       applied or curried functions.
 * [ ] Selectively importing functions from other modules.
 * [ ] Aliasing imported functions and modules. 
 * [ ] Compiler optimizations for common patterns in functions and data structures. 

## Objectives

 * Efficiency and performance: Fjord is strictly evaluated and should box as 
   little as possible. Unique types and values, similar to Rust, allow us to 
   leverage the speed of mutable objects without suffering the bugs and 
   concurrency issues of shared mutable state.
 * Fast compilation: a fast compiler is a great productivity aid. A project 
   written in Fjord should compile in less than a second.
 * Usability: Fjord should be usable on both the frontend and the backend,
   allowing for code sharing. 
