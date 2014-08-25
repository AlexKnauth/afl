afl
===

a meta-language for adding rackjure-like [anonymous function literals](http://www.greghendershott.com/rackjure/index.html#%28part._func-lit%29) to a language, based on at-exp and rackjure

documentation: http://pkg-build.racket-lang.org/doc/afl/index.html

[![Build Status](https://travis-ci.org/AlexKnauth/afl.png?branch=master)](https://travis-ci.org/AlexKnauth/afl)

Example:
```racket
#lang afl racket/base
(map #λ(+ % 1) '(1 2 3)) ;=> '(2 3 4)
```
