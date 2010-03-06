(define (gcd a b)
  (if (= b 0)
     a
     (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((num (/ n g))
          (den (/ d g)))
      (cond ((negative? den) (cons (- num) (- den)))
            (else (cons num den))))))