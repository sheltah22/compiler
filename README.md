# Some Compiler
#### Author: Shelby Frazier

## Overview
This is a compiler for a toy languages whose features I have not fully decided on yet.

## Current Language
#### Types
  - Integers (ex. `1`, `-12`)
  - Functions (ex. `(fun (y:int) : int -> (y + 2))`)
  - Unit type (ex. `()`)
  - Tuples (ex. `(true, 1) : (bool * int)`)
  - Lists (ex. `[], 2 :: 1 :: []`)
  - Reference (ex. `(ref 100) : <int>`)

#### Operations
  - Basic Arithmetic: +, -, \*, / (ex. `1 + 1`)
  - Integer Comparison: <, >, <=, >=, = \t (ex. `1 <= 1`)
  - If statements: (ex. `if 0 = 0 then 10 else 11`)
  - Let bindings: (ex. `let (x : int) = 1 in x + 1`)
  - Function definitions: (ex. `fun (x : bool) : int -> if x then 1 else 0`)
  - Recursive functions: (ex. `fix f (n : int) : int -> if n = 0 then 1 else n * f(n - 1)`)
  - Function application: (ex. `(fun (x : bool) : int -> if x then 1 else 0) true`)
  - Pair projection: (ex. `(1, 3, false)[1]`)
  - Lists Operators:
    + Cons: (ex. `1 :: 3 :: 5 :: []`)
    + Head: (ex. `hd (1 :: 3 :: 5 :: []:int)`)
    + Tail: (ex. `tl (1 :: 3 :: 5 :: []:int)`)
    + Empty predicate: (`empty? []`)
  - Stateful Operators:
    + Creation: (ex. `let x : <int> = (ref 10)`)
    + Assignment: (ex. `x := 10 + 2`) \* expression on right hand side is converted to a ref type, returns `()`
    + Bang: retrieves value of reference (ex. `!x`)
    + Sequencing operator: returns the value of the last expression (ex. `x := 10; !x` returns 10)
    + While loop: (ex. `while (!x < 2) do x := !x + 1 end !x`)

#### Other Features
 - Static typechecking

#### Known bugs/things to be changed
- Multi-parameter functions can only be built with nested higher-order functions
- Must use parenthesis when calling a function with multiple parameters: `(f x) y`

#### Future work
- Implement a simple type inference system
- Add user-defined types + polymorphism with ML typechecking
- Add pattern matching
- Improve syntax for defining functions

## Building and Testing
### Setup Instructions
You will need ocaml, opam and oasis installed to run the compiler.
1. Install ocaml and opam using your preferred package manager.
2. Use opam to install oasis and menhir using `opam install oasis` and
   `opam install menhir`.

### Build Instructions
Run `make clean` to clean up the directory (if necessary), and run `make` to build the project.

### Execution Instructions
The compiler can be run using `./compiler.native`.

## Changelog

#### Assignment 7 (Final)
1. New Features
  - Pairs replaced with tuples of arbitrary length and syntax
    `(a, b, c) : (A * B * C)`
2. Changes to existing features
  - Added negative integers
3. Known bugs
  - [No new known bugs]

#### Assignment 6
1. New Features
  - Added a notion of state/environment to execution
  - added operators `ref v` returns a reference to value v
                    `!x` dereferences x
                    `x := y` assignment operator
                    `x;y` sequencing operator
                    `while e1 do e2 end` while loop
2. Changes to existing features
  - Binary operators are now left associative, official
    precedence levels established for others
3. Known bugs
  - Still must parenthesize function calls with multiple parameters in the
    same way

#### Assignment 5
1. New Features
  - Typechecking done before attempting to run the program,
    Types include int, bool, t1 -> t2, unit, t1 * t2, [t] (a homogeneous list)
  - Pairs of the form `(x,y)` added
  - Homogeneous lists of the form `(x :: [] : t)` added
  - Functions (as keywords) pertaining to lists and pairs: fst, snd (pairs) and hd, tl, empty? (lists)
2. Changes to existing features
  - syntax for let bindings and function declarations updated to include types:
    `let x : t = e1 in e2`
    `fun (x:t1) : t2 -> e`
    `fix f (x:t1) : t2 -> e2`
3. Known Bugs
  - Must use parenthesis when calling a function with multiple parameters: `(f x) y`

#### Assignment 4
1. New Features
  - Let bindings of the form `let x = 10 in ...`
  - Function definitions of the form `fun x -> x + x`
  - Recursive function definitions of the form `fix f x -> if ... then ... else 1 + (f (x - 1))`
  - Now uses small step evaluation, can be visualized using the -step flag
2. Changes to existing features
  - Some error messages have been altered
3. Known Bugs
  - Spaces must be used as a delimiter for binary operations with variables. For example, `x+1` will result in an error; use `x + 1` instead

#### Assignment 3
1. New Features
  - if statements now have lazy execution
2. Changes to existing features
  - All binary operators are now infix operators
  - Changed `if` statement syntax to include `then` and `else` keywords
  - e ::= e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2
      | e1 <= e2 | if e1 then e2 else e3 | e_base
  - e_base ::= n | true | false | (exp)
  - Precedence of operators:
    + Parenthesized expressions
    + All binary operations /*/* (right associative)
    + Conditionals
3. Known Bugs
  [No known bugs yet]

#### Assignment 2
1. New Features
  - `./compiler.native <file.arith>` can be used to compile and print the result computed by a program using an S-expression language with the following syntax:
  - e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
        | true | false | (<= e1 e2) | (if e1 e2 e3)
2. Changes to existing features
  - No longer prints command line arguments, or uses -length flag to print length of command line arguments
3. Known Bugs
  - If statements evaluate all branches before checking conditional


#### Assignment 1
1. New Features
  - `./compiler.native` will print any command line arguments given by the user, one per line.
  - the `-length` flag can be used to print the length of each command line argument instead.
2. Changes to existing features
  [No previously existing features yet]
3. Known Bugs
  [No known bugs yet]
