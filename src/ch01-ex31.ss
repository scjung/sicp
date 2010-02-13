;;; a.
;;;
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity n) n)

(define (factorial n) (product identity 1 inc n))

(define (approx-pi n)
  (define (term n)
    (/ (+ n (if (even? n) 2 1))
       (+ n (if (even? n) 1 2))))
  (* 4 (product term 1 inc n)))

;;; b.
;;;
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))