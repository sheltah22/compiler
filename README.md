# Some Compiler
#### Author: Shelby Frazier

## Overview
This is a compiler for a toy languages whose features I have not fully decided on yet.

## Language Specifications
TBD.

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
    precedency levels established for others
3. Known bugs
  - Still must parenthesize funtion calls with multiple parameters in the
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
    + All binary operations **(right associative)
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
