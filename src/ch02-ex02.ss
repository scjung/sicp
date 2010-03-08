(define (average x y) (/ (+ x y) 2))

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))