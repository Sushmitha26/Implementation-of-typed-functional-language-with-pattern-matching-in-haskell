# Typed Functional Language with Pattern Matching

A small typed functional programming language implemented in Haskell, with support for parsing, static type checking, interpretation, list pattern matching, and recursive functions.

## Overview

This project was built as part of a programming languages course to connect core PL concepts with a working implementation. The language includes basic functional constructs such as lambda expressions, function application, let-bindings, lists, pattern matching, and recursive functions through `letrec`.

The implementation is divided into separate modules for syntax, parsing, type checking, evaluation, and top-level execution.

## Features

- Integer and Boolean literals
- Arithmetic and comparison operators
- Conditional expressions
- Non-recursive bindings with `let`
- Recursive function bindings with `letrec`
- Lambda expressions with type annotations
- Function application
- Lists using `Nil` and `Cons`
- Pattern matching with `case`
- Static type checking
- Big-step interpreter
- Multi-line program input

## Language Constructs

### Types
- `Int`
- `Bool`
- `List T`
- `T1 -> T2`

### Expressions
- Integer literals: `1`, `2`, `42`
- Boolean literals: `true`, `false`
- Arithmetic: `+`, `-`, `*`
- Comparisons: `==`, `<`
- Conditionals:
  ```text
  if true then 1 else 0
