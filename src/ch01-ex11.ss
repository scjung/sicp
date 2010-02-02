;;; recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;;; iterative process
(define (f n)
  (define (f-iter counter fn1 fn2 fn3)
    (if (> counter n)
        fn1
        (f-iter (+ counter 1)
                (+ fn1 (* 2 fn2) (* 3 fn3))
                fn1
                fn2)))
  (if (< n 3)
      n
      (f-iter 3 2 1 0)))
        
  