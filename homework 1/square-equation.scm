(define (square-equation a b c)
  (if (= a 0) 
      (/ (- c) b)
      (if (< (- (expt b 2) (* 4 a c)) 0)
          '()
          (let* ((d (- (expt b 2) (* 4 a c)))
                 (x1 (/ (- (+ b (sqrt d))) (* 2 a)))
                 (x2 (/ (+ (- b) (sqrt d)) (* 2 a))))
            (if (= x1 x2) (list x1)
                (list x1 x2))))))