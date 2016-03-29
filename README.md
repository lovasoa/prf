# prf
Define primitive recursive functions in a clear unambiguous language, check them for correctness, and execute them.
See https://en.wikipedia.org/wiki/Primitive_recursive_function

## Language définition
### predefined functions
syntax | function | function name
--- | --- | ---
`C0` | `() -> 0` | null constant
`S`  | `(x) -> x+1` | successor function
`P i n` | `(x1, x2, ... , xn) -> xi` | projector
### Function composition
syntax | function | function name
--- | --- | ---
`Compose f [g1, g2,..., gn]` | `(x1,...,xm) -> f(g1(x1,...,xm),... ,gn(x1,...,xm))` | function composition
### Recursive function definition
syntax | function | function name
--- | --- | ---
`Recurse f g` | `f (k, x1,...,xm) -> if k == 0 then f(x1,...,xm) else g(k-1, f(k-1, x1,...,xm), x1,...,xm)` | recursive function

## Example
### Addition
```hs
Recurse (P 1 1) (Compose S [P 2 3])
```
### Multiplication
```hs
Recurse (Recurse C0 (P 2 2)) (Compose (Recurse (P 1 1) (Compose S [P 2 3])) [P 3 3, P 2 3])
```

## Program usage
This repo hosts an implementation for this language. You can download it from the **release** section.
### Command-line
`$ ./prf function_file.prf`

The program will then read function arguments from the standard input.
