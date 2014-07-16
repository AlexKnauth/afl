#lang scribble/manual

@(require scribble/eval
          (for-label (except-in racket/base
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

@deftogether[(@defproc[(afl-read [in input-port? (current-input-port)]
                                 [#:arg-str arg-str string? (current-arg-string)]) any]{}
              @defproc[(afl-read-syntax [source-name any/c (object-name in)]
                                        [in input-port? (current-input-port)]
                                        [#:arg-str arg-str string? (current-arg-string)])
                       (or/c syntax? eof-object?)]{})]{
These procedures implement the @racketmodname[afl] reader.  They do so by
constructing a readtable based on the current one, and using that
for reading.

The @racket[arg-str] argument lets you specify something else to use as a placeholder instead of
@racket[%].

@examples[
  (require afl/reader)
  (afl-read (open-input-string "#λ(+ % %2)"))
  (afl-read (open-input-string "#λ(+ _ _2)") #:arg-str "_")
]

@racketmodname[afl/reader] also exports these functions under the names @racket[read] and
@racket[read-syntax].
}

@defproc[(make-afl-readtable [orig-readtable readtable? (current-readtable)]
                             [#:arg-str arg-str string? (current-arg-string)]) readtable?]{
makes an @racketmodname[afl] readtable based on @racket[orig-readtable].

The @racket[arg-str] argument lets you specify something else to use as a placeholder instead of
@racket[%], just like for @racket[afl-read].
}

@defproc[(use-afl-readtable [orig-readtable readtable? (current-readtable)]
                            [#:arg-str arg-str string? (current-arg-string)]) void?]{
passes arguments to @racket[make-afl-readtable] and sets the @racket[current-readtable] parameter to
the resulting readtable.
It also enables line counting for the @racket[current-input-port] via @racket[port-count-lines!].

This is mostly useful for the REPL.

@verbatim{
Examples:

> @racket[(require afl/reader)]
> @racket[(use-afl-readtable)]
> @racket[(map @#,afl[(+ % %2)] '(1 2 3) '(1 2 3))]
@racketresult['(2 4 6)]
> @racket[(use-afl-readtable #:arg-str "_")]
> @racket[(map @#,afl[(+ _ @#,racketid[__2])] '(1 2 3) '(1 2 3))]
@racketresult['(2 4 6)]
}}

@defparam[current-arg-string arg-str string?]{
a parameter that controls default values of the @racket[arg-str] arguments to @racket[afl-read] etc.
}