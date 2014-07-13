afl
===

a meta-language for rackjure-like anonymous function literals, based on at-exp and rackjure

Example:
```racket
#lang afl racket/base
(map #λ(+ % 1) '(1 2 3)) ;=> '(2 3 4)
```
