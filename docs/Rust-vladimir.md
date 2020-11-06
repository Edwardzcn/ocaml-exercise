# Macros in rust

brief introduction

## Features of rust

Why we use rust not C/C++.

Modern language use a lot of new things.

- Zero cost abstractopm
- Error messages
- Type inference
- Move semantics
  - Don't d
- Threads without data races (Wooooo!)
  - owner and ? 
  - rust comiler will think if safe
- Pattern matching
- Guaranteed memory safety
- Efficient C bindings
- Safe memory space allocation

## Rust Traits

Trats are kind of similar to interfaces in OOP languages(Java).

Rust can uses these。


## Declarative Macros

Macros are created using the `macro_rules!` exp.

`macro_rules!` is a syntax extension, that uses the following form (rules)


## Captures


## Repetition

- `$` is a literal doller token.

...

## Debugging Macros

When developing complex macros,

expended_macros

log_sumtax

## Procedural Macros

allow creating syntax extensions as execution of a function 

- function-like macros - `custom!(...)`
- Derive macros - `#[derive(CustomDerive)]`
- Attribute macros - `#[CustomAttribute]`

You can think of procedural macros as functions from AST to AST