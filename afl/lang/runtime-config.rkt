#lang racket/base

(provide configure)

(require (only-in afl/reader make-afl-readtable))

(define (configure data)
  (current-readtable (make-afl-readtable)))

