;; Вспомогательные функции:

; Асимптотика: O(n), где n - длина списка xs
(define (contains? el xs)
  (and (not (null? xs))
       (or (equal? el (car xs))
           (contains? el (cdr xs)))))

; Асимптотика: O(n), где n - длина списка xs
(define (count el xs)
  (if (null? xs)
      0
      (if (equal? el (car xs))
          (+ 1 (count el (cdr xs)))
          (count el (cdr xs)))))

;;№1
; Асимптотика: O(n^2), где n - длина списка xs
(define (list->set xs)
  (define (loop xs ans)
    (if (null? xs)
        ans
        (if (contains? (car xs) ans)
            (loop (cdr xs) ans)
            (loop (cdr xs) (cons (car xs) ans)))))
  (loop xs (list)))

;;№2
; Асимптотика: O(n^2), где n - длинна списка xs
(define (set? xs)
  (or (null? xs)
      (and (= (count (car xs) xs) 1)
           (set? (cdr xs)))))

;;№3
; Асимптотика: O(n^2), где n - длинна списка,
; полученного после append'а xs и ys
(define (union xs ys)
  (list->set (append xs ys)))

;;№4
; Асимптотика: O(n^2), где n - длина списка xs
(define (intersection xs ys)
  (if (null? xs)
      (list)
      (if (contains? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

;;№5
; Асимптотика: O(n^2), где n - длина списка xs
(define (difference xs ys)
  (if (null? xs)
      (list)
      (if (contains? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))

;;№6
; Асимптотика: O(n^2), где n - длина списка,
; полученная в результате append'а разности
; множеств xs и ys; ys и xs
(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

;;№7
; Асимптотика: O(n^2), где n совпадает с n в №6
(define (set-eq? xs ys)
  (and (equal? (list) (symmetric-difference xs ys))))
