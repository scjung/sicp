(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (upper-bound y))))))

(define (mul-interval x y)
  (let ((lx (lower-bound x)) (ux (upper-bound x))
        (ly (lower-bound y)) (uy (upper-bound y)))
    (if (>= ux 0)
        (if (>= lx 0)
            (if (>= uy 0)
                (if (>= ly 0)
                    (make-interval (* lx ly) (* ux uy))  ; lx,ux,ly,uy >= 0
                    (make-interval (* ly ux) (* ux uy))) ; lx,ux,uy >= 0; ly < 0
                (make-interval (* ux ly) (* lx uy)))     ; lx,ux >= 0; ly,uy < 0
            (if (>= uy 0)
                (if (>= ly 0)
                    (make-interval (* lx uy) (* ux uy))  ; ux,ly,uy >= 0; lx < 0
                    (let ((p1 (* lx ly))                 ; ux,uy >= 0; lx,ly < 0
                          (p2 (* lx uy))
                          (p3 (* ux ly))
                          (p4 (* ux uy)))
                      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
                (make-interval (* ux ly) (* lx ly))))    ; ux >= 0; lx,ly,uy < 0
        (if (>= uy 0)
            (if (>= ly 0)
                (make-interval (* lx uy) (* ux ly))      ; lx,ux < 0; ly,uy >= 0
                (make-interval (* lx uy) (* lx ly)))     ; lx,ux,ly < 0; uy >= 0
            (make-interval (* ux uy) (* lx ly))))))      ; lx,ux,ly,uy < 0