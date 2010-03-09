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

;; a rectangle is constructed from top-left point and bottom-right point.
;; top-left: *-----o
;;           |     |
;;           o-----* :bottom-right

(define (make-rect top-left bottom-right)
  (cons (cons top-left
              (make-point (x-point bottom-right)
                          (y-point top-left)))
        (cons (make-point (x-point top-left)
                          (y-point bottom-right))
               bottom-right)))

(define (top-left-rect r) (car (car r)))

(define (top-right-rect r) (cdr (car r)))

(define (bottom-left-rect r) (car (cdr r)))

(define (bottom-right-rect r) (cdr (cdr r)))

(define (top-segment-rect r)
  (make-segment (top-left-rect r) (top-right-rect r)))

(define (bottom-segment-rect r)
  (make-segment (bottom-left-rect r) (bottom-right-rect r)))

(define (left-segment-rect r)
  (make-segment (top-left-rect r) (bottom-left-rect r)))

(define (right-segment-rect r)
  (make-segment (top-right-rect r) (bottom-right-rect r)))

(define (width-rect r)
  (let ((horizontal (top-segment-rect r)))
    (- (x-point (end-segment horizontal))
       (x-point (start-segment horizontal)))))

(define (height-rect r)
  (let ((vertical (left-segment-rect r)))
    (- (y-point (end-segment vertical))
       (y-point (start-segment vertical)))))

(define (perimeter-rect r)
  (* (+ (width-rect r) (height-rect r)) 2))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))