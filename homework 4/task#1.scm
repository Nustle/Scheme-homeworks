;(use-syntax (ice-9 syncase))

(define memoized-factorial
  (let ((memo '() ))
    (lambda (n)
      (let ((memoized (assoc n memo)))
        (if memoized
            (cadr memoized)
            (let ((new-value (if (< n 2)
                                 1
                                 (* n (memoized-factorial (- n 1))))))
              (set! memo (cons (list n new-value) memo))
              new-value))))))
