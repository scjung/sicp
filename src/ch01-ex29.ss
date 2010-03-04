(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define r (remainder a 3))
    (* (cond ((= k 0) 1)
             ((= k n) 1)
             ((even? k) 2)
             (else 4))
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum term 0 inc n)))
