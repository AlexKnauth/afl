#lang racket/base

(provide configure)

(require (only-in afl/reader use-afl-readtable))

(define (configure data)
  (use-afl-readtable))

