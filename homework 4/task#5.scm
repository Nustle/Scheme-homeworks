(define break #f)

(define-syntax define-data
  (syntax-rules ()
    ((_ data_title ((title argument ...) ...))
     (begin
       (eval (list 'define 'title (lambda (argument ...)
                      (list (list 'a-type 'data_title)
                            (list 'd-type 'title) argument ...)))
             (interaction-environment)) ...
       (eval (list 'define
                   (string->symbol (string-append (symbol->string 'data_title) "?"))
                   (lambda (x)
                       (if (and (list? x) (>= (length x) 2))
                        (if (pair? (car x))
                          (and (equal? 'data_title (cadr (assoc 'a-type x))))
                          break)
                      break))) (interaction-environment))))))


(define-syntax match
  (syntax-rules ()
    ((_ x ((title argument ...) expr) ...)
     (cond
       ((equal? (cadadr x) 'title)
        (apply (lambda (argument ...) expr) (cddr x))
        )
       ...
       (else x)))))
