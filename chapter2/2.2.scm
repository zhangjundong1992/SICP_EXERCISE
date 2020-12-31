#| 平面上线段表示 |#
(define (average x y)
    (/ (+ x y) 2))

(define (midpoint-segment seg)
    (make-point (average    (x-point (start-segment seg)) 
                            (x-point (end-segment seg)))
                (average    (y-point (start-segment seg))
                            (y-point (end-segment seg)))))

(define (make-segment start end)
    (cons start end))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

; (define p1 (make-point 1 2))
; (define p2 (make-point 3 4))
; (define seg (make-segment p1 p2))
; (midpoint-segment seg)
