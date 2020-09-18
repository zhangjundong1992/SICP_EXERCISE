(define (mul a b)
    (cond   ((= b 0) 0)
            ((even? b) (double (mul a (halve b))))
            ((odd? b) (+ a (mul a (- b 1))))))

(define (double x)
    (+ x x))

(define (halve x)
    (/ x 2))    