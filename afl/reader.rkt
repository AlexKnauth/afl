#lang racket/base

(provide make-afl-readtable
         afl-read
         afl-read-syntax
         wrap-reader
         (rename-out
          [afl-read read]
          [afl-read-syntax read-syntax])
         )

(require racket/match
         rackjure/threading
         (only-in racket/port input-port-append)
         (only-in racket/list remove-duplicates append*)
         (for-template racket/base)
         (for-syntax racket/base))

(define (afl-read [in (current-input-port)])
  (define orig-readtable (current-readtable))
  (parameterize ([current-readtable (make-afl-readtable orig-readtable)])
    (read in)))

(define (afl-read-syntax [src (object-name (current-input-port))] [in (current-input-port)])
  (define orig-readtable (current-readtable))
  (parameterize ([current-readtable (make-afl-readtable orig-readtable)])
    (read-syntax src in)))

(define (wrap-reader p)
  (lambda args
    (define orig-readtable (current-readtable))
    (parameterize ([current-readtable (make-afl-readtable orig-readtable)])
      (apply p args))))

(define (make-afl-readtable [orig-rt (current-readtable)] #:arg-char [arg-char #\%])
  (define reader-proc (make-reader-proc orig-rt #:arg-char arg-char))
  (let* ([rt orig-rt]
         [rt (make-readtable rt #\λ 'dispatch-macro reader-proc)]
         [rt (make-readtable rt #\f 'dispatch-macro reader-proc)]
         [rt (make-readtable rt #\l 'dispatch-macro reader-proc)])
    rt))



(define ((make-reader-proc [orig-rt (current-readtable)] #:arg-char [arg-char #\%])
         char in source line column pos)
  (define (unget-normal-read-syntax str src in)
    (define rt (current-readtable))
    (parameterize ([current-readtable orig-rt])
      (read-syntax/recursive src (input-port-append #f (open-input-string str) in) #f rt)))
  (define (peek/read? str in)
    (and (equal? str (peek-string (string-length str) 0 in))
         (read-string (string-length str) in)))
  (cond [(char=? char #\l)
         (cond [(peek/read? "ambda" in) (parse (read-syntax source in) #:arg-char arg-char)]
               [else (unget-normal-read-syntax "#l" source in)])]
        [(char=? char #\f)
         (cond [(peek/read? "n" in) (parse (read-syntax source in) #:arg-char arg-char)]
               [(peek/read? "unction" in) (parse (read-syntax source in) #:arg-char arg-char)]
               [else (unget-normal-read-syntax "#f" source in)])]
        [(char=? char #\λ) (parse (read-syntax source in) #:arg-char arg-char)]
        ;[else (unget-normal-read-syntax (string #\# char) source in)]
        [else (parse (read-syntax source in) #:arg-char arg-char)] ;single letter e.g. #λ
        ))

(define (parse stx #:arg-char [arg-char #\%])
  (define arg-str (string arg-char))
  (define (string->id stx . strs)
    (datum->syntax stx (string->symbol (apply string-append strs)) stx))
  (with-syntax ([args (parse-args stx #:arg-char arg-char)]
                [% (string->id #'args arg-str)]
                [%1 (string->id #'args arg-str "1")]
                [body stx])
    #'(lambda args
        (define-syntax % (make-rename-transformer #'%1))
        body)))

(module+ test
  (require rackunit)
  ;; These test `parse`. See test.rkt for tests of readtable use per se.
  (define chk (compose1 syntax->datum parse))
  (check-equal? (chk #'(+))
                '(lambda ()
                  (define-syntax % (make-rename-transformer #'%1))
                  (+)))
  (check-equal? (chk #'(+ 2 %1 %1))
                '(lambda (%1)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %1 %1)))
  (check-equal? (chk #'(+ 2 %3 %2 %1))
                '(lambda (%1 %2 %3)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %3 %2 %1)))
  (check-equal? (chk #'(apply list* % %&))
                '(lambda (%1 . %&)
                  (define-syntax % (make-rename-transformer #'%1))
                  (apply list* % %&))))

;; parse-args : Stx -> KW-Formals-Stx
(define (parse-args stx #:arg-char arg-char)
  ;; Filter the stxs to those that start with %, 
  ;; find the maximum, find whether there are any
  ;; keyword arguments or a rest argument, and
  ;; produce kw-formals based on that.
  (define arg-str (string arg-char))
  (define-values (max-num rest? kws)
    (find-arg-info stx #:arg-char arg-char))
  (define datum-kw-formals
    (append (for/list ([n (in-range 1 (add1 max-num))])
              (string->symbol (string-append arg-str (number->string n))))
            (append*
             (for/list ([kw (in-list kws)])
               (list kw (string->symbol (string-append arg-str "#:" (keyword->string kw))))))
            (cond [rest? (string->symbol (string-append arg-str "&"))]
                  [else '()])))
  (datum->syntax stx datum-kw-formals stx))

;; find-arg-info : Any -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info v #:arg-char arg-char)
  (match (maybe-syntax-e v)
    [(? symbol? sym) (find-arg-info/sym sym #:arg-char arg-char)]
    [(? pair? pair)  (find-arg-info/pair pair #:arg-char arg-char)]
    [_               (return)]))

;; find-arg-info/sym : Symbol -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/sym sym #:arg-char arg-char)
  (define (arg-char? char)
    (char=? char arg-char))
  (match (~> sym symbol->string string->list)
    [(list)                     (return)]
    [(list  (? arg-char?))      (return #:max-num 1)]
    [(list  (? arg-char?) #\&)  (return #:rest? #t)]
    [(list* (? arg-char?) #\# #\: cs)
     (return #:kws (~> cs list->string string->keyword list))]
    [(list (? arg-char?) (? char-numeric? cs) ...)
     (return #:max-num (~> cs list->string string->number))]
    [_ (return)]))

;; find-arg-info/pair :
;;   (Cons Symbol Symbol) -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/pair pair #:arg-char arg-char)
  (define-values (car.max-num car.rest? car.kws)
    (find-arg-info (car pair) #:arg-char arg-char))
  (define-values (cdr.max-num cdr.rest? cdr.kws)
    (find-arg-info (cdr pair) #:arg-char arg-char))
  (return #:max-num (max car.max-num cdr.max-num)
          #:rest? (or car.rest? cdr.rest?)
          #:kws (remove-duplicates (append car.kws cdr.kws))))

(define (return #:max-num [max-num 0] #:rest? [rest? #f] #:kws [kws '()])
  (values max-num rest? kws))

(define (maybe-syntax-e stx)
  (cond [(syntax? stx) (syntax-e stx)]
        [else stx]))
