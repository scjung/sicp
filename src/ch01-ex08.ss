(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  
(define (good-enough? old-guess guess)
  (< (abs (- old-guess guess)) 0.00001))

(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess
                 (improve guess x)
                 x)))

(define (cube-root x)
  (sqrt-iter 1.0 (improve 1.0 x) x))