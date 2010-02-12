(define (square n) (* n n))

(define (nontrivial-square-root? a n)
  (cond ((= a 1) #f)
        ((= a (- n 1)) #f)
        ((= (remainder (square a) n) 1) #t)
        (else #f)))

(define (expmod base exp m)
  (cond ((nontrivial-square-root? base m) 0)
        ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (define e (expmod a n n))
    (and (not (= e 0)) (= e a)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
