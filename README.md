# Cherry Lisp

```
  /\
 |  \  A tiny, curried and lazy Lisp.
 @   @
```

## Curried? Lazy?

Currying consists in convert **one function** that takes **multiple arguments** into **multiple functions** that each take a **single argument**.

*WIP*

Lazyness is when an expression is only evaluated **when is necessary**.

## Features

- Interpreted
- Dynamic typed
- Functional (impure)
- Curried
- Lazy evaluation
- It's a Lisp

## Inspiration

This language is being created for fun.

It takes inspiration of some features of *Haskell* (like currification and lazy evaluation) and the syntax of *Lisp*.

## A small taste

``` lisp
cherry> (set! x 10)
nil
cherry> x
10
cherry> (const 3 6)
3
cherry> (set! always-seven (const 7))
nil
cherry> (always-seven 1)
7
cherry> (always-seven 8)
7
```

## Download and build instructions

Install the *stack* build tool. On *Debian* based systems, run (as root):

```
# apt install haskell-stack
```

Clone the repo:

```
$ git clone https://git.fai.su/Suguivy/cherry-lisp

```

Compile it:

```
$ cd cherry-lisp
$ stack upgrade
$ stack build
```

Install and execute it:

```
$ stack install
$ cherry
```

*Work in progress...*
