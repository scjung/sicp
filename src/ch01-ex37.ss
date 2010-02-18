(define (cont-frac n d k)
  (define (term i)    
    (/ (n i) 
       (if (= i k)
           (d i)
           (+ (d i) (term (inc i))))))
  (term 1))

(define (cont-frac-iter n d k)
  (define (term i result)
    (if (= i 0)
        result
        (term (- i 1) (/ (n i) (+ (d i) result)))))
  (term (- k 1) (/ (n k) (d k))))