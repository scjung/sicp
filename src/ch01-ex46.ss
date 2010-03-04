(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt x)
  (define (good-enough? old guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))