# prf
Define primitive recursive functions in a clear unambiguous language, check them for correctness, and execute them.
See https://en.wikipedia.org/wiki/Primitive_recursive_function

## Language définition
### predefined functions
syntax | function | function name
--- | --- | ---
`C0` | `() -> 0` | null constant
`S`  | `(x) -> x+1` | successor function
`Pi:n`|  `(x1, x2, ... , xn) -> xi` | projector
### Function composition
syntax | function | function name
--- | --- | ---
`f(g1, g2,..., gn)` | `(x1,...,xm) -> f(g1(x1,...,xm),... ,gn(x1,...,xm))` | function composition
### Recursive function definition
syntax | function | function name
--- | --- | ---
`f = {g|h}` | `f(0, x1,...,xm) = g(x1,...,xm)` and `f(k+1, x1,...,xm) = h(k, f(k, x1,...,xm), x1,...,xm)` | recursive function

## Example
### Addition
```hs
identity = P1:1
addition = { identity | S(P2:3) }
addition
```
### Multiplication
```hs
identity = P1:1
addition = { identity | S(P2:3) }
null = { C0 | P2:2 }
multiplication = { null | addition(P2:3,P3:3) }
multiplication
```

You'll find more examples in the [`examples/`](https://github.com/lovasoa/prf/tree/master/examples) directory.

## Program usage
This repo hosts an implementation for this language. You can download it from the **release** section.
### Command-line
`$ ./prf function_file.prf`

The program will then read function arguments from the standard input.
