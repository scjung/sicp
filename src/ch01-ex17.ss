(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (even? n) (= (remainder n 2) 0))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))
