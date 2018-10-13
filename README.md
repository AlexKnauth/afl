afl [![Build Status](https://travis-ci.org/AlexKnauth/afl.png?branch=master)](https://travis-ci.org/AlexKnauth/afl)
===

a lang-extension for adding rackjure-like [anonymous function literals](https://docs.racket-lang.org/rackjure/index.html#%28part._func-lit%29) to a language, based on at-exp and rackjure

documentation: https://docs.racket-lang.org/afl/index.html

Example:
```racket
#lang afl racket/base
(map #λ(+ % 1) '(1 2 3)) ;=> '(2 3 4)
```

Comparison with similar packages / alternatives:

| Package                                                            | Example                           | Multi-Args               |
| ------------------------------------------------------------------ | --------------------------------- | ------------------------ |
| [afl](https://docs.racket-lang.org/afl/index.html)                 | `(map #λ(+ % 1) (list 1 2 3))`    | `%1`, `%2` etc.          |
| [aful](https://docs.racket-lang.org/aful/index.html)               | `(map #λ(+ % 1) (list 1 2 3))`    | `%1`, `%2` etc.          |
| [fancy-app](https://docs.racket-lang.org/fancy-app/index.html)     | `(map (+ _ 1) (list 1 2 3))`      | `_`, `_` in same order   |
| [curly-fn](https://docs.racket-lang.org/curly-fn/index.html)       | `(map #{+ % 1} (list 1 2 3))`     | `%1`, `%2` etc.          |
| [rackjure](https://docs.racket-lang.org/rackjure/index.html)       | `(map #λ(+ % 1) (list 1 2 3))`    | `%1`, `%2` etc.          |
| [srfi/26](https://docs.racket-lang.org/srfi/srfi-std/srfi-26.html) | `(map (cut + <> 1) (list 1 2 3))` | `<>`, `<>` in same order |

