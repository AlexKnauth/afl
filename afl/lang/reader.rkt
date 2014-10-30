(module reader racket/base
  (require syntax/module-reader
           (only-in "../reader.rkt" make-afl-readtable wrap-reader))
  
  (provide (rename-out [afl-read read]
                       [afl-read-syntax read-syntax]
                       [afl-get-info get-info]))
  
  (define-values (afl-read afl-read-syntax afl-get-info)
    (make-meta-reader
     'afl
     "language path"
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     (lambda (orig-read-syntax)
       (define read-syntax (wrap-reader orig-read-syntax))
       (lambda args
         (syntax-property (apply read-syntax args)
                          'module-language
                          '#(afl/lang/language-info get-language-info #f))))
     (lambda (proc)
       (lambda (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (define (try-dynamic-require mod export)
           (or (with-handlers ([exn:fail? (λ (x) #f)])
                 (dynamic-require mod export))
               (fallback)))
         (case key
           [(color-lexer)
            (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
           [(definitions-text-surrogate)
            'scribble/private/indentation]
           [else (fallback)]))))))
