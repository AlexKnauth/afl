#lang racket/base

(provide make-afl-readtable
         afl-read
         afl-read-syntax
         wrap-reader
         use-afl-readtable
         current-arg-string
         (rename-out
          [afl-read read]
          [afl-read-syntax read-syntax])
         )

(require racket/match
         rackjure/threading
         racket/port
         racket/list
         racket/function
         syntax/srcloc
         hygienic-reader-extension/extend-reader
         (for-meta -10 racket/base)
         (for-meta -9 racket/base)
         (for-meta -8 racket/base)
         (for-meta -7 racket/base)
         (for-meta -6 racket/base)
         (for-meta -5 racket/base)
         (for-meta -4 racket/base)
         (for-meta -3 racket/base)
         (for-meta -2 racket/base)
         (for-meta -1 racket/base)
         (for-meta 0 racket/base)
         (for-meta 1 racket/base)
         (for-meta 2 racket/base)
         (for-meta 3 racket/base)
         (for-meta 4 racket/base)
         (for-meta 5 racket/base)
         (for-meta 6 racket/base)
         (for-meta 7 racket/base)
         (for-meta 8 racket/base)
         (for-meta 9 racket/base)
         (for-meta 10 racket/base)
         (for-meta 11 (only-in racket/base #%app make-rename-transformer syntax))
         )

(module+ test
  (require rackunit))

(define (afl-read [in (current-input-port)] #:arg-str [arg-str (current-arg-string)])
  (parameterize ([current-arg-string arg-str])
    ((wrap-reader read) in)))

(define (afl-read-syntax [src (object-name (current-input-port))] [in (current-input-port)]
                         #:arg-str [arg-str (current-arg-string)])
  (parameterize ([current-arg-string arg-str])
    ((wrap-reader read-syntax) src in)))

(define (wrap-reader p)
  (extend-reader p make-afl-readtable))

(define (make-afl-readtable [orig-rt (current-readtable)]
                            #:outer-scope outer-scope
                            #:arg-str [arg-str (current-arg-string)])
  (define reader-proc (make-reader-proc orig-rt outer-scope #:arg-str arg-str))
  (let* ([rt orig-rt]
         [rt (make-readtable rt #\λ 'dispatch-macro reader-proc)]
         [rt (make-readtable rt #\f 'dispatch-macro reader-proc)]
         [rt (make-readtable rt #\l 'dispatch-macro reader-proc)])
    rt))

(define (use-afl-readtable [orig-rt (current-readtable)] #:arg-str [arg-str (current-arg-string)])
  (port-count-lines! (current-input-port))
  (current-readtable (make-afl-readtable orig-rt #:outer-scope identity #:arg-str arg-str)))

(define current-arg-string (make-parameter "%"))


(module+ test
  (check-equal? (afl-read (open-input-string "#λ(+ % %2)"))
                '(lambda (%1 %2)
                   (define-syntax % (make-rename-transformer #'%1))
                   (+ % %2)))
  (check-equal? (afl-read (open-input-string "#λ(+ _ _2)") #:arg-str "_")
                '(lambda (_1 _2)
                   (define-syntax _ (make-rename-transformer #'_1))
                   (+ _ _2)))
  )


(define ((make-reader-proc orig-rt outer-scope #:arg-str [arg-str (current-arg-string)])
         char in src ln col pos)
  (parameterize ([current-arg-string arg-str])
    (define (unget-normal-read-syntax str src in)
      (define rt (current-readtable))
      (parameterize ([current-readtable orig-rt])
        (read-syntax/recursive src (input-port-append #f (open-input-string str) in) #f rt)))
    (define (peek/read? str in)
      (and (equal? str (peek-string (string-length str) 0 in))
           (read-string (string-length str) in)))
    (cond [(char=? char #\l)
           (cond [(peek/read? "ambda" in)
                  (define stx (read-syntax src in))
                  (parse stx outer-scope
                         #:loc (srcloc src ln col pos (- (source-location-end stx) pos)))]
                 [else (unget-normal-read-syntax "#l" src in)])]
          [(char=? char #\f)
           (cond [(peek/read? "n" in)
                  (define stx (read-syntax src in))
                  (parse stx outer-scope
                         #:loc (srcloc src ln col pos (- (source-location-end stx) pos)))]
                 [(peek/read? "unction" in)
                  (define stx (read-syntax src in))
                  (parse stx outer-scope
                         #:loc (srcloc src ln col pos (- (source-location-end stx) pos)))]
                 [else (unget-normal-read-syntax "#f" src in)])]
          [(char=? char #\λ)
           (define stx (read-syntax src in))
           (parse stx outer-scope
                  #:loc (srcloc src ln col pos (- (source-location-end stx) pos)))]
          ;[else (unget-normal-read-syntax (string #\# char) source in)]
          [else ;single letter e.g. #λ
           (define stx (read-syntax src in))
           (parse stx outer-scope
                  #:loc (srcloc src ln col pos (- (source-location-end stx) pos)))]
          )))

(define (parse stx outer-scope #:loc [loc stx] #:arg-str [arg-str (current-arg-string)])
  (parameterize ([current-arg-string arg-str])
    (define (string->id stx . strs)
      (datum->syntax stx (string->symbol (apply string-append strs)) stx))
    (match-define (srcloc src ln col pos spn) (build-source-location loc))
    (define stx-pos (syntax-position stx))
    (define loc-stx (build-source-location-syntax loc))
    (define λ-loc
      (update-source-location loc-stx
        #:column (and col (+ col 1))
        #:position (and pos (+ pos 1))
        #:span (and stx-pos pos (max 0 (- stx-pos pos 1)))))
    (hygienic-app
     #:outer-scope outer-scope
     (lambda (stx*)
       (with-syntax ([lambda (orig (syntax/loc λ-loc lambda))]
                     [args (parse-args stx* #:arg-str arg-str)]
                     [% (string->id stx* arg-str)]
                     [%1 (string->id stx* arg-str "1")]
                     [body stx*])
         (syntax/loc loc-stx
           (lambda args
             (define-syntax % (make-rename-transformer #'%1))
             body))))
     stx)))

(define (orig stx)
  (syntax-property stx 'original-for-check-syntax #t))

(module+ test
  ;; These test `parse`. See test.rkt for tests of readtable use per se.
  (define (chk stx)
    (syntax->datum (parse stx identity)))
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
                  (apply list* % %&)))
  (check-equal? (parameterize ([current-arg-string "_"])
                  (chk #'(apply list* _ _&)))
                '(lambda (_1 . _&)
                  (define-syntax _ (make-rename-transformer #'_1))
                  (apply list* _ _&))))

;; parse-args : Stx -> KW-Formals-Stx
(define (parse-args stx #:arg-str [arg-str (current-arg-string)])
  ;; Filter the stxs to those that start with %, 
  ;; find the maximum, find whether there are any
  ;; keyword arguments or a rest argument, and
  ;; produce kw-formals based on that.
  (parameterize ([current-arg-string arg-str])
    (define-values (max-num rest? kws)
      (find-arg-info stx))
    (define datum-kw-formals
      (append (for/list ([n (in-range 1 (add1 max-num))])
                (string->symbol (string-append arg-str (number->string n))))
              (append*
               (for/list ([kw (in-list kws)])
                 (list kw (string->symbol (string-append arg-str "#:" (keyword->string kw))))))
              (cond [rest? (string->symbol (string-append arg-str "&"))]
                    [else '()])))
    (datum->syntax stx datum-kw-formals stx)))

;; find-arg-info : Any -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info v)
  (match (maybe-syntax-e v)
    [(? symbol? sym) (find-arg-info/sym sym)]
    [(? pair? pair)  (find-arg-info/pair pair)]
    [_               (return)]))

;; find-arg-info/sym : Symbol -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/sym sym)
  (define arg-str (current-arg-string))
  (define (arg-str? str)
    (string=? str arg-str))
  (define (arg-cs? cs)
    (arg-str? (~> cs list->string)))
  (define str (~> sym symbol->string))
  (match (~> str string->list)
    [(list)                  (return)]
    [_ #:when (arg-str? str) (return #:max-num 1)]
    [(list  arg-cs ... #\&)
     #:when (arg-cs? arg-cs)
     (return #:rest? #t)]
    [(list* arg-cs ... #\# #\: kw-cs)
     #:when (arg-cs? arg-cs)
     (return #:kws (~> kw-cs list->string string->keyword list))]
    [(list arg-cs ... (? char-numeric? n-cs) ...)
     #:when (arg-cs? arg-cs)
     (return #:max-num (~> n-cs list->string string->number))]
    [_ (return)]))

;; find-arg-info/pair :
;;   (Cons Symbol Symbol) -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/pair pair)
  (define-values (car.max-num car.rest? car.kws)
    (find-arg-info (car pair)))
  (define-values (cdr.max-num cdr.rest? cdr.kws)
    (find-arg-info (cdr pair)))
  (return #:max-num (max car.max-num cdr.max-num)
          #:rest? (or car.rest? cdr.rest?)
          #:kws (remove-duplicates (append car.kws cdr.kws))))

(define (return #:max-num [max-num 0] #:rest? [rest? #f] #:kws [kws '()])
  (values max-num rest? kws))

(define (maybe-syntax-e stx)
  (cond [(syntax? stx) (syntax-e stx)]
        [else stx]))
