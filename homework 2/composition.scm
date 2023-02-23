(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)))))

#| Ачивка: |#

(define (my-fold-left op xs)
  (if (= 1 (length xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

; Используем my-fold-left из hw2-lists
(define (o . xs)
  (lambda (x)
    (my-fold-left
     (lambda (x op)
       (op x))
     (cons x (reverse xs))))
