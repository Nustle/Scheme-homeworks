
; Асимптотика: O(n), где n - длинна строки str
(define (string->reverse str)
  (list->string (reverse (string->list str))))

; Асимптотика: O(n^2), где n - кол-во пробелов слева в строке
(define (string-trim-left str)
  (if (char-whitespace? (car (string->list str)))
      (string-trim-left (list->string (cdr (string->list str))))
      str))

; Асимптотика: O(n^2), где n - кол-во пробелов справа в строке
(define (string-trim-right str)
  (string->reverse (string-trim-left (string->reverse str))))

; Асимптотика: O(n^2), где n - длинна строки
(define (string-trim str)
  (string-trim-right(string-trim-left str)))

; Асимптотика: O(n^2), где n - длинна строки a
(define (string-prefix? a b)
  (and (<= (string-length a) (string-length b))      
       (let ((t (map
                 (lambda (x y) (equal? x y))
                 (string->list a)
                 (string->list b)
                 )))
         (define (loop eq-lst)
           (or (null? eq-lst)
               (and (car eq-lst)
                    (loop (cdr eq-lst)))))
         (loop t))))

; Асимптотика: O(n^2), где n - длинна строки a
(define (string-suffix? a b)
  (string-prefix? (string->reverse a) (string->reverse b)))

; Асимптотика: O(n^2), где n - длинна строки a
(define (string-infix? a b)
  (and (> (string-length b) 0)
       (or (string-prefix? a b)
           (string-infix? a (substring b 1)))))

; Асимптотика: O(n^2), где n - длинна строки str
(define (string-split str sep)
  (if (= 0 (string-length str))
      '()
      (if (string-prefix? sep str)
          (string-split (substring str (string-length sep)) sep)
          (cons (substring str 0 1) (string-split (substring str 1) sep)))))

#|  Ачивки  |#

; strim-trim-right, удаляющая пробельные символы без реверса.
; Асимптотика: O(n^2)
(define (string-trim-right str)
  (if (char-whitespace? (string-ref str (- (string-length str) 1)))
      (string-trim-right (substring str 0 (- (string-length str) 1)))
      str))

#|
