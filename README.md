# Myy lambda calculus interpreter

Simply-typed lambda calculus interpreter. Intended to be easy-to-use and help users learn about the lambda calculus.

## Installing

You can install `myy` by cloning this repository and installing via cabal.

```
git clone https://github.com/japiirainen/myy
---
cd myy
---
cabal install exe:myy
---
```

## Usage examples

The previous steps produced an executable `myy` You can start an myy repl session by running the executable.

Here is an example session.

```
➜  myy git:(main) ✗ myy
                   \\\\\\
                    \\\\\\
                     \\\\\\
                      \\\\\\
                       \\\\\\
                      //\\\\\\
                     ////\\\\\\
                    //////\\\\\\
                   //////  \\\\\\
                  //////    \\\\\\
Welcome to the Myy interpreter, version 1.0.
λ> (\x: Int . \y: Int . x + y) 30 39
69 : Int

λ> not = \b: Bool . if b then false else true
not = λ#. if #0 then false else true : Bool -> Bool

λ> not (5 % 2 == 0)
true : Bool

λ> app (3 < 4) not
Bad function application.
   Left-hand type: (Int -> Int) -> Int -> Int
  Right-hand type: Bool
in the expression 'app (3 < 4)'

λ> reverseApp = (\x:Int . \y: Int -> Int . y x)
reverseApp = λ#. λ#. #0 #1 : Int -> (Int -> Int) -> Int

λ> :step app (\x:Int . x + 1) (4 % 2)
(λ#. λ#. #1 #0) (λ#. #0 + 1) (4 % 2) : Int
--> (λ#. (λ#. #0 + 1) #0) (4 % 2) : Int
--> (λ#. #0 + 1) (4 % 2) : Int
--> 4 % 2 + 1 : Int
--> 0 + 1 : Int
--> 1 : Int
1 : Int
:quit
Good-bye.
```

As you can see above the input to myy must be fully annotated. Myy does not support type inference.

## The Language

The myy language is an explicitly typed lambda calculus with integers (`Int`) and booleans (`Bool`). Also the following operators are supported, with their usual meaning.

```
+ - * / % < <= > >= ==
```

Myy supports a ternary conditional operator, demonstrated in the snippet above, as `if <boolean expression> then <exp> else <exp>`.

Boolean constants are spelled `false` and `true`.

## Commands

`:quit` quits the myy interpreter.

`:lex` lexes the given text and pretty-prints the result.

`:parse` parses the given text and pretty-prints the result.

`:eval` type-checks and evaluates the given expression. This is the default behaviour at the command line.

`:step` runs the given expression through the simple-step semantics. This shows you every step of the way from your expression down to a value. This uses different evaluation strategy that `:eval` does, but the result should always be the same.

`:type` gives you the type of an expression.

`:all` runs both `:eval` `:step` on an expression.