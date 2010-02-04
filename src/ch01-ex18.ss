(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (even? n) (= (remainder n 2) 0))

(define (fast-mul a b)
  (define (mul r a b)
    (cond ((= b 0) 0)
          ((= b 1) (+ r a))
          ((even? b) (mul r (double a) (halve b)))
          (else (mul (+ r a) a (- b 1)))))
  (mul 0 a b))