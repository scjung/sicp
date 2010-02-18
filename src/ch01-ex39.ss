(define (cont-frac n d k)
  (define (term i result)
    (if (= i 0)
        result
        (term (- i 1) (/ (n i) (+ (d i) result)))))
  (term (- k 1) (/ (n k) (d k))))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* i 2) 1))
             k))