(define break #f)

(call-with-current-continuation
 (lambda (k) (set! break k)))

(define force-return 0)
(define (exit reason)
  (force-return #f))

(define (push xs x)
  (append xs (list x)))

(define (make-source sequence . end)
  (define xs (cond ((list? sequence) sequence)
                   ((vector? sequence) (vector->list sequence))
                   ((string? sequence) (string->list sequence))))
  (define eot (if (pair? end)
                  (car end)
                  #f)) 
  (list->vector (cons 2 (cons eot xs))))

(define (peek vs)
  (if (= (vector-ref vs 0) (vector-length vs))
      (vector-ref vs 1)
      (vector-ref vs (vector-ref vs 0))))

(define (next vs)
  (if (= (vector-ref vs 0) (vector-length vs))
      (vector-ref vs 1)
      (begin
        (vector-set! vs 0 (+ 1 (vector-ref vs 0))) 
        (vector-ref vs (- (vector-ref vs 0) 1)))))

(define (space? sym) 
  (or (eq? #\tab sym)
      (eq? #\space sym)
      (eq? #\newline sym)))

(define (int? num)
  (or (eq? #\1 num)
      (eq? #\2 num)
      (eq? #\3 num)
      (eq? #\4 num)
      (eq? #\5 num)
      (eq? #\6 num)
      (eq? #\7 num)
      (eq? #\8 num)
      (eq? #\9 num)
      (eq? #\0 num)
      (eq? #\. num)))

(define (bracket? sym)
  (or (eq? #\( sym)
      (eq? #\) sym)))

(define (op? sym)
  (or (eq? #\- sym)
      (eq? #\+ sym)
      (eq? #\* sym)
      (eq? #\/ sym)
      (eq? #\^ sym)))
(define (AddOp? sym)
  (or (eq? #\- sym)
      (eq? #\+ sym)))
(define (MulOp? sym)
  (or (eq? #\/ sym)
      (eq? #\* sym )))
(define (E? sym)
  (or
   (eq? sym #\e)
   (eq? sym #\E)))

;; №1
; БНФ:
; <выражение> ::= <пробелы> <объект> <пробелы> <выражение> | <пусто>
; <пробелы> ::= ПРОБЕЛ <пробелы> | <пусто>
; <объект>  ::= (
;            | )
;            | +
;            | -
;            | *
;            | /
;            | ^
;            | <переменная>
;            | <число>         
; <число> ::= ЦИФРА <хвост-числа>
; <хвост-числа> ::= ЦИФРА <хвост-числа> | e <хвост-числа> | . <хвост-числа> | <пусто>
; <переменная> ::= БУКВА <хвост-переменной>
; <хвост-переменной> ::= БУКВА <хвост-переменной> | <пусто>
; <пусто>            ::=

(define (tokenize str)
  (define src (make-source str))
  (define (lexer xs var num)
    (cond ((and (pair? num) (or (not (peek src)) (and
                                                  (not (int? (peek src)))
                                                  (not (E? (peek src)))
                                                  (not (and (AddOp? (peek src))
                                                            (E? (car num)))))))
           (lexer (cons (string->number (list->string (reverse num))) xs) var '()))
          ((and (pair? var) (or (not (peek src)) (not (char-alphabetic? (peek src)))))
           (lexer (cons (string->symbol (list->string (reverse var))) xs) '() num))
          ((not (peek src)) (reverse xs))
          ((and (E? (peek src))
                (pair? num))
           (lexer xs var (cons (next src) num)))
          ((and (AddOp? (peek src))
                (pair? num))
           (lexer xs var (cons (next src) num)))
          ((char-alphabetic? (peek src))
           (lexer xs (cons (next src) var) num))
          ((int? (peek src))
           (lexer xs var (cons (next src) num)))
          ((bracket? (peek src))
           (lexer (cons (string (next src)) xs) var num))
          ((op? (peek src))
           (lexer (cons (string->symbol (string (next src))) xs) var num))
          ((space? (peek src))
           (begin
             (next src)
             (lexer xs var num)))
          (else #f)))
  (lexer '() '() '()))

;; №2

; Expr ::= Term Expr' .
; Expr' ::= AddOp Term Expr' | .
; Term ::= Factor Term' .
; Term' ::= MulOp Factor Term' | .
; Factor ::= Power Factor' .
; Factor' ::= PowOp Power Factor' | .
; Power ::= value | "(" Expr ")" | unaryMinus Power .
(define (parse tokens)
  (define index 0)
  (define bracket-index #f)
  (define (inc) (set! index (+ 1 index)))
  (define (in-bounds?) (< index (length tokens)))
  (define (current) (list-ref tokens index))

  (define (has-close-bracket?)
    (let loop ((index index) (offset 0))
      (and (not (= index (length tokens)))
           (let ((current (list-ref tokens index)))
             (cond
               ((and (equal? current ")") (zero? offset)))
               ((equal? current "(") (loop (+ index 1) (+ offset 1)))
               ((equal? current ")") (loop (+ index 1) (- offset 1)))
               (else (loop (+ index 1) offset)))))))
  
  (define (expression open-bracket-index)
    (let loop ((T (term)))
      (if (and (in-bounds?) (or (equal? (current) '-) (equal? (current) '+)))
          (let ((op (current)))
            (inc)
            (if (not (in-bounds?)) (exit "expr1"))
            (loop (list T op (term))))
          (if (in-bounds?)
              (if (and (equal? (current) ")") open-bracket-index)
                  (begin (inc) T)
                  (exit "expr2"))
              T))))
        
  (define (term)
    (let loop ((F (factor)))
      (if (and (in-bounds?) (or (equal? (current) '/) (equal? (current) '*)))
          (let ((op (current)))
            (inc)
            (loop (list F op (factor))))
          F)))
          

  (define (factor)
    (let ((P (power)))
      (if (and (in-bounds?) (equal? (current) '^))
          (begin (inc) (list P '^ (factor)))
          P)))

  (define (power)
    (if (not (in-bounds?))(exit "power1"))
    (let ((current (current)))
      (inc)
      (cond
        ((equal? current '-) (list '- (power)))
        ((equal? current "(") (if (has-close-bracket?)
                                  (expression (- index 1))
                                  (exit "power2")))
        ((number? current) current)
        ((symbol? current) current)
        (else (exit "power3")))))

  (call-with-current-continuation
   (lambda (stack)
     (set! force-return stack)
     (expression #f))))


;; №3

(define (tree->scheme tree)
  (if (and (list? tree) (= (length tree) 3))
      (let ((a (car tree)) (op (cadr tree)) (b (caddr tree)))
        (let ((op (if (equal? op '^) 'expt op)))
          (list op (tree->scheme a) (tree->scheme b))))
      tree))
