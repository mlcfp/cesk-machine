# CESK Machine

BNF grammar for ANF

```
<prog> ::= <dec> ...

<dec> ::=  (define <var> <exp>)
        |  (begin <dec> ...)
        |  <exp>

lam ::= (λ (var1 ... varN) exp)

aexp ::=  lam
       |  var
       |  #t  |  #f
       |  integer
       |  (prim aexp1 ... aexpN)

cexp ::=  (aexp0 aexp1 ... aexpN)
       |  (if aexp exp exp)
       |  (call/cc aexp)
       |  (set! var aexp)
       |  (letrec ((var1 aexp1) ... (varN aexpN)) exp)

exp ::=  aexp
      |  cexp
      |  (let ((var exp)) exp)

prim ::=  +  |  -  |  *  |  =
```

## References

Matt Might has written much on CESK machines.

- [Writing an interpreter, CESK-style](https://matt.might.net/articles/cesk-machines/)
- [A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)

Matthias Felleisen invented the CESK machine.

- [Principles of Programming Languages](https://felleisen.org/matthias/4400-s20/index.html)

This presentation had a nice diagram for the garbage collector.

- [Copying Garbage Collection](https://users.cs.northwestern.edu/~stamourv/teaching/321-F19/16a-gc-copying.pdf)


## New Functional Language

Name ideas
- Havoc
- Mahem (could be abbreviate `mh` which is close to `ml`)

## TODO

X ANF parser
X scheme to ANF normalizer
X scheme AST to ANF AST conversion
X scheme renderer
X anf renderer
- add string and char values
- add word values (bytes?)
- add float values and float division
- add logical operators
- add pair values for cons, lists, etc
- add `begin` to dec lexer
X garbage collector
X top level defines
- CPS transform (see micinski)
- tail call optimization
- add builtins for IO etc
- add ML syntax
- add type system and type inference
- add ADT or GADT
- modules
- exceptions
- concurrency
  - async/await
  - virtual threads
  - message passing actors?
- relational programming support
  - microkanren?
