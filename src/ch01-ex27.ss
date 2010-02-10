(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (carmichael-num? n)
  (define (test a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (test (+ a 1)))
          (else false)))
  (and (not (prime? n)) (test 1)))
