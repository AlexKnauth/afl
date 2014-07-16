#lang scribble/manual

@(require (for-label (except-in racket/base
                                read read-syntax)
                     (except-in afl/reader
                                read read-syntax)))

@title{afl}

@;; example: @racket[(map @#,afl[(+ % 1)] '(1 2 3))]
@(define-syntax-rule @afl[form ...]
   @elem{@tt{#λ}@racket[form ...]})

@section{#lang afl}

@defmodulelang[afl]{
The @racketmodname[afl] language is a meta-language like @racketmodname[at-exp]
that adds @racketmodname[rackjure]-like anonymous function literals to a language.  
@margin-note{see @secref["func-lit" #:doc '(lib "rackjure/rackjure.scrbl")]}

For example, @racket[@#,hash-lang[] @#,racketmodname[afl] @#,racketmodname[racket/base]]
adds anonymous function literals to @racketmodname[racket/base], so that
@racketmod[afl @#,racketmodname[racket/base]
(map @#,afl[(+ % 1)] '(1 2 3))]
produces @racket['(2 3 4)]
}

@section{afl/reader}

@defmodule[afl/reader]

@deftogether[(@defproc[(afl-read [in input-port? (current-input-port)]) any]{}
              @defproc[(afl-read-syntax [source-name any/c (object-name in)] [in input-port? (current-input-port)]) (or/c syntax? eof-object?)]{})]{
These procedures implement the @racketmodname[afl] reader.  They do so by
constructing a readtable based on the current one, and using that
for reading.

@racketmodname[afl/reader] also exports these functions under the names @racket[read] and @racket[read-syntax].
}

@defproc[(make-afl-readtable [orig-readtable readtable? (current-readtable)]) readtable?]{
makes an @racketmodname[afl] readtable based on @racket[orig-readtable].
}
