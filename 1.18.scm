;用加法模拟乘法，迭代计算过程
(define (mul a b)
    (mul-iter a b 0)
)

(define (mul-iter a b res)
    (cond   ((= b 0) res)
            ((even? b) (mul-iter (double a) (halve b) res))
            ((odd? b) (mul-iter a (- b 1) (+ res a)))
    )
)

(define (double x)
    (+ x x)
)

(define (halve x)
    (/ x 2)
)    