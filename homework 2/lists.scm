;; №1
; Асимптотика: O(n), где n=b-a+1
(define (my-range a b d)
  (if (>= (+ a d) b)
      (list a)
      (cons a (my-range (+ a d) b d))))

;; №2
; Асимптотика: O(n^2), где n - длинна списка xs
; так как append имеет линейную сложность и всего n проходов
(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (append (list (car xs)) (my-flatten (cdr xs))))))

;; №3
; Асимптотика: O(n) где n - длинна списка xs
(define (my-element? x xs)
  (and (not (null? xs))
       (or (equal? (car xs) x) (my-element? x (cdr xs)))))

;; №4
; Асимптотика: O(n) где n - длинна списка xs
(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

;; №5
; Асимптотика: O(n^2) где n - длинна списка xs
(define (my-fold-left op xs)
  (if (= 1 (length xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

;; №6
; Асимптотика: O(n^2) где n - длинна списка xs
(define (my-fold-right op xs)
  (if (= 1 (length xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

#| Ачивки: |#

; 1. my-flatten без append.
;    Из-за loop(loop(...)) не является хвостовой
;    Асимптотика: O(n^2) где n - длинна списка xs
;    так как каждому рекуррентному вызову соответсвует
;    вложенный в него рекуррентный вызов
(define (my-flatten xs)
  (define (loop ans xs)
    (if (null? xs)
        ans
        (if (list? xs)
            (loop (loop ans (cdr xs)) (car xs))
            (cons xs ans))))
  (loop (list) xs))

; 2. my-flatten без append с хвостовой рекурсией.
;    Асимптотика: O(n^2) где n - длинна списка xs
;    так как после раскрытия вложенных списков за линейное время
;    требуется сделать reverse массива, который делается за O(n)
(define (my-flatten xs)
  (define (loop ans xs)
    (if (null? xs)
        ans
        (if (list? (car xs))
            (if (> (length (car xs)) 0)
                (loop ans (cons (caar xs) (cons (cdar xs) (cdr xs))))
                (loop ans (cdr xs)))
            (loop (cons (car xs) ans) (cdr xs)))))
  (if (list? xs)
      (reverse (loop (list) xs))
      (reverse (list xs))))

; 3. my-flatten без append с хвостовой рекурсией 
;    Асимптотика: O(n)
(define (my-flatten xs)
  (let loop ((xs xs) (stack '()))
    (cond
      ((and (null? xs) (null? stack)) '())
      ((and (null? xs) (not(null? stack))) (loop (car stack) (cdr stack)))
      ((pair? (car xs)) (loop (car xs) (cons (cdr xs) stack)))
      (else (cons (car xs) (loop (cdr xs) stack))))))
