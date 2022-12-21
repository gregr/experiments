# miniVerse (and microVerse)

This is an implementation of VC based on the paper: [The Verse Calculus: a Core Calculus for Functional Logic Programming](https://simon.peytonjones.org/assets/pdfs/verse-conf.pdf)

This implementation uses a naive sequence-concurrent evaluation strategy
that should be complete with respect to the rewrite semantics from the paper.
Runnable examples from the paper are included.

[micro.rkt](micro.rkt) provides a small-step interpreter for a low-level VC
notation.  [example-micro.rkt](example-micro.rkt) contains examples written
with the micro notation.

[mini.rkt](mini.rkt) builds on micro, providing some syntactic sugar for
writing VC programs, as well as a small base library.
[example-mini.rkt](example-mini.rkt) contains examples written with the
mini notation.

## Getting started

- [Download Racket](https://download.racket-lang.org/)

- Verify that everything is working (no obvious errors or crashes) by running the examples found in this directory.  You can run them from the command line like this:
  - `racket example-micro.rkt`
  - `racket example-mini.rkt`

## Running miniVerse programs

While writing a Racket program, or while running Racket interactively, `(require "mini.rkt")` to import procedures for running miniVerse programs.

The most convenient procedures to use are these:

- `(run* PROGRAM)` will run `PROGRAM` until it produces a `value` or gets stuck.
  If `PROGRAM` diverges or loops forever, `run*` may not terminate.
- `(run STEP-COUNT PROGRAM)` is like `(run* PROGRAM)`, except it only performs
  `STEP-COUNT` steps of execution.  If this is not enough to produce a value, the
  current execution state will be returned.  `run` should always terminate.
- `(trace-run* PROGRAM)` is like `(run* PROGRAM)`, except it produces a trace of
  all the intermediate execution states of the program being run.  These states
  may be quite verbose, even for small programs.
- `(trace-run STEP-COUNT PROGRAM)` is the tracing variant of `(run STEP-COUNT PROGRAM)`.

Here is an example of using `run*` to run miniVerse programs within Racket's interactive environment:

```
> racket
Welcome to Racket v8.4 [cs].

> (require "mini.rkt")

> (run* #t)
(value #t)

> (run* '(cons 'a 'b))
(value (a . b))

> (run* '(all (exist (X)
                (== X (alt 1 2))
                (cons X X))))
(value #((1 . 1) (2 . 2)))

> (run* '(exist (first)
           (== first (lambda (x)
                       (exist (a b) (== x (cons a b)) a)))
           (exist (x y)
             (== x (cons y 5))
             (== (first x) 2)
             y)))
(value 2)

> (run* '(all (exist (x y)
                (== x (alt 3 4))
                (== y (alt 20 30))
                (cons x y))))
(value #((3 . 20) (3 . 30) (4 . 20) (4 . 30)))

> (run* '(all (exist (x)
                (== (vector-ref (vector 2 3 2 7 9) x) 2)
                x)))
(value #(0 2))

> (run* '(all (exist (append)
                (== append
                    (lambda (xs ys)
                      (alt
                        (begin (== xs '()) ys)
                        (exist (x xrest)
                          (== xs (cons x xrest))
                          (cons x (append xrest ys))))))
                (exist (as bs)
                  (== (append as bs) (list 1 2 3))
                  (list as bs)))))
(value #((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ())))
```

## Running microVerse programs

There is no convenient interface provided for running microVerse programs yet.
For now, take a look at the scaffolding in [example-micro.rkt](example-micro.rkt)
to see one possible way to run them.

One reason you might want to run microVerse programs directly, at least while
experimenting, is because their execution states are currently much less verbose
than those of the corresponding miniVerse programs, making them easier to follow.
(This can be fixed for miniVerse programs.  See the TODO list.)

## TODO

- Make tracing less verbose by eliminating unused dependencies in states
  - garbage collection (we currently do none)
  - safe-for-space closures that only capture actual dependencies, rather than everything they see

- Experiment with different kinds of IO
  - immediate read/write on a channel: sort of like unsafePerformIO
  - scoped IO: an IO handler that receives and services IO request descriptions, sort of like the IO monad
  - transactional IO: a (mozart/oz-style) single-assignment stream of IO requests is gradually assigned to by concurrent processes

- Try more sophisticated evaluation strategies for better performance
