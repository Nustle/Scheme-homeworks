; 1)
(define (make-multi-vector sizes . fill) 
  (append
   (list (cons 'multi-vector sizes))
   (list (if (= (length fill) 1)
             (make-vector (apply * sizes) (car fill))
             (make-vector (apply * sizes))))))
#|

Пример вызова процедуры make-multi-vector:
> (define m (make-multi-vector '(2 2 3) 1))
> m
> ((multi-vector 2 2 3) #(1 1 1 1 1 1 1 1 1 1 1 1))

|#

; 2)
(define (multi-vector? m)
  (and (list? m)
       (equal? (caar m) 'multi-vector)
       (vector? (cadr m))))

#|

Пример вызова процедуры multi-vector?:
> (define m (make-multi-vector '(2 2 1) 9))
> m
((multi-vector 2 2 1) #(9 9 9 9))
> (multi-vector? m)
#t

> (define ms (list '(2 2 1) #(8 8 8)))
> ms
((2 2 1) #(8 8 8))
> (multi-vector? ms)
#f

> (define mk (list '(2 2 1) '(7 7 7)))
> mk
((2 2 1) (7 7 7))
> (multi-vector? mk)
#f

> (define ml (list (cons 'multi-vector '(2 2 1)) '(7 7 7)))
> ml
((multi-vector 2 2 1) (7 7 7))
> (multi-vector? ml)
#f

|#

; Вспомогательная процедурв, возвращающая
; индекс вектора через индекс мультивектора
(define (get-index all-sizes indices)
  (define (index-position sizes indices)
    (if (null? (cdr sizes))
        (car sizes)
        (+ (index-position (cdr sizes) (cdr indices))
           (* (car indices) (apply * (cdr sizes))))))
  (index-position (cdr all-sizes) indices))

; 4)
(define (multi-vector-set! m indices elem)
  (vector-set! (cadr m) (get-index (car m) indices) elem))

#|

Пример вызова процедуры multi-vector-set!:
> (define m (make-multi-vector '(2 2 3)))
> m
> ((multi-vector 2 2 3) #(0 0 0 0 0 0 0 0 0 0 0 0))
> (multi-vector-set! m '(1 0 1) 10)
> m
((multi-vector 2 2 3) #(0 0 0 0 0 0 0 0 0 10 0 0))

|#

; 5)
(define (multi-vector-ref m indices)
  (vector-ref (cadr m) (get-index (car m) indices)))

#|

Пример вызова процедуры multi-vector-set!:
> (define m (make-multi-vector '(2 2 3)))
> m
> ((multi-vector 2 2 3) #(0 0 0 0 0 0 0 0 0 0 0 0))
> (multi-vector-set! m '(1 0 1) 10)
> m
((multi-vector 2 2 3) #(0 0 0 0 0 0 0 0 0 10 0 0))
> (multi-vector-ref m '(1 0 1))
10

|#
