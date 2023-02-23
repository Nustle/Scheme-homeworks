;; Определение дня недели по дате с помощью алгоритма JD1
(define (day-of-week d m y)
  (let* ((a (quotient (- 14 m) 12))
         (b (- y a))
         (c (+ m (- 2) (* 12 a)))
         (e (+ 7000 d b (quotient b 4) (quotient b 400)))
         (weekDay (+ e (- (quotient b 100)) (quotient (* 31 c) 12))))
    (if (equal? (remainder weekDay 7) 0)
        7
        (remainder weekDay 7))))
