(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (if (= (remainder n 3) 0)
      (car (/ n 3))
      (/ (log n) (log 2))))

(define (cdr n)
  (if (= (remainder n 2) 0)
      (cdr (/ n 2))
      (/ (log n) (log 3))))