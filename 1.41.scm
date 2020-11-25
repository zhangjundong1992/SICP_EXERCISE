;将f过程应用两次
(define (double f)
    (lambda (x) (f (f x)))
)

(define (inc x)
    (+ x 1)
)

;((double inc) 1)