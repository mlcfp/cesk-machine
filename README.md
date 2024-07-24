# CESK Machine

The purpose of this project is to explore the CESK machine as an
interpreter for functional languages and continuations for advanced
control flow.  The project should culminate with
the implementation of a ML style, statically typed functional
language that runs on the CESK machine.  In the end, the interpreters
should be suitable for small, general purpose programs.

## References

Matt Might has written much on CESK machines.

- [Writing an interpreter, CESK-style](https://matt.might.net/articles/cesk-machines/)
- [A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)

Matthias Felleisen invented the CESK machine.

- [Principles of Programming Languages](https://felleisen.org/matthias/4400-s20/index.html)

This presentation has a nice diagram for two-space garbage collection.

- [Copying Garbage Collection](https://users.cs.northwestern.edu/~stamourv/teaching/321-F19/16a-gc-copying.pdf)


## TODO

- clean up compile warnings
- add word values (bytes?)
- add array/vector
- tail call optimization
- add builtins for IO etc
- add intrinsics for date and time
- add delimited continuations
- add repl
- add ML syntax
- add type system and type inference
- exceptions
- concurrency
- CPS transform (see micinski)

## DONE

- ANF parser
- scheme to ANF normalizer
- scheme AST to ANF AST conversion
- scheme renderer
- anf renderer
- add float values and float division
- add logical operators
- garbage collector
- top level defines
- add string and char values
- add builtins for type test (char?, etc)
- add builtins for numerics
- add error codes to cesk machine rather than just text
- add signed numbers
- add pair values for cons, lists, etc
- add `begin` to dec lexer
