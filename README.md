# mp-scheme

### What is it

`mp-scheme` is my own personal attempt at writing a functional scheme-like interpreter in Haskell, using minimal guides. So yes, it is definitely not the best implementation around, but it has some neat little features to make it customizable to my liking. Here is some of the current functionality:

Types supported:

* `Integers`
* `Doubles`
* `Booleans`
* `Strings`
* `Lists`
* `Closures`
* `Chars`

Features:

* Functions defined by the `define` keyword are globally scoped, and as a result can be called anywhere (and support recursion)
* Lists can be defined by the following operators:
    * `(list 1 2 3)`
    * `'(1 2 3)`
    * `[1 2 3]`
* `cond` supports n arguments, where the last argument is the else case. Here is some example syntax:
```scheme 
(cond [(= 1 2) #t] [(= 4 5) #f] [(= 2 3) (list 1 2 3)] [else "else case"])
```
* `and`, `or`, `=`, `<`, `>`, `<=`, `>=`, `+`, `-`, `/`, `*`, support anywhere from 2 to n arguments
```scheme
(and #t #t) => #t
(and #t #t #f) => #f
(or #t #f #t) => #t
(or #f #f #f) => #f
(= 1 1 1 1) => #t
(= 1 2 3 4) => #f
(<= 1 2 3 4) => #t
(<= 5 2 1) => #f
```
* Here is a full list of built in operands:
    * `and`
    * `or`
    * `not`
    * `*`, `+`, `-`, `/`
    * `=`, `<`, `>`, `<=`, `>=`
    * `if` and `cond`
    * `lambda` with n arguments
    * `cons`
    * `append`
    * `first`
    * `rest`
    
See the standard library `stdlib.scm` for details.
    

