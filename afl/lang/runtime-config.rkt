#lang racket/base

(provide configure)

(require (only-in afl/reader make-afl-readtable))

(define (configure data)
  (define old-read (current-read-interaction))
  (define (new-read src in)
    (parameterize ([current-readtable (make-afl-readtable (current-readtable))])
      (old-read src in)))
  (current-read-interaction new-read))
