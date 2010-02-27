(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (floor (log2 n)))
                            1.0))