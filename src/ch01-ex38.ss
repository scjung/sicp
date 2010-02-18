(define (cont-frac n d k)
  (define (term i result)
    (if (= i 0)
        result
        (term (- i 1) (/ (n i) (+ (d i) result)))))
  (term (- k 1) (/ (n k) (d k))))

(define (approx-e k)
  (+ 2.0
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (- i (floor (/ i 3)))
                        ; 'floor' cuts out the fractional part
                      1.0))
                k)))