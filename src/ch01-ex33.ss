(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (if (filter a)
                  (combiner (term a) result)
                  result))))
  (iter a null-value))

(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
     a
     (gcd b (remainder a b))))

; a.
;
(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

; b.
;
(define (product-rel-primes n)
  (define (rel-prime? i) (= (gcd i n) 1))
  (filtered-accumulate * rel-prime? 1 identity 1 inc (- n 1)))