# EEL - Haskell Embedded Embedded Language

Eel is an embedded domain specific language (EDSL) for writing
resource constrained programs at a high level.  The idea is to write
code at a high level in the pure functional language Haskell which is
then transformed by the eel DSL into (efficient!) low level LLVM code
which can then be fed into clang.

This is a toy project for exploring/learning about DSLs and low level
code generation.

The current plan is to build this with layered DSLs, as follows:

## Eel-LLVM

Essentially a one-to-one mapping between the Haskell code and LLVM
code.  Lifting this code into Haskell already adds some benefits:

* Names can be generated automatically (LLVM code uses SSA form):
  E.g., labels.

* Types can be inferred: E.g., alloca (adhoc)polymorphism: E.g. 'add'
  in eel-llvm vs. 'add' and 'fadd' in LLVM.

* Lighter syntax: E.g., no need for 'call' instruction.

* Type safe Macros: Essentially we can use the host language, Haskell,
  as a type safe macro language.

## Eel-C (TBD)

A layering of Haskell constructs to lift eel-llvm up to roughly the
expressivity of C (hopefully more expressive).  This will allow, for
example, nested sub-expressions, if statements, loops, structs,
user-defined datatypes.

## Eel-Pure (TBD)

The goal here is to write in a pure, functional style and yet still
generate what amounts to a state machine in LLVM code.

## Other goals:

* No dynamic memory or garbage collection: Sizes and memory layouts
  will be determined statically, at compile time.

* No recursion: Stack usage will be statically determined.

* No exceptions: Something like Haskell's Either type will be used
  instead.

* Eliminate undefined behavior: Either the Haskell type system or
  other static analysis tools will be empolyed to do as much as is
  feasible here.

* Be competitive with C as far as time/space: Getting example
  benchmarks (e.g. Computer Language Benchmarks Game) up and running
  will help inform this.
