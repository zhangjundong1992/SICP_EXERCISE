;迭代计算过程求解斐波那契数列
(define (fib n)
    (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
    (cond   ((= count 0) b)
            ((even? count) 
                (fib-iter a b 
                            (+ (square p) (square q))
                            (+ (* 2 p q) (square q)) (/ count 2)
                )
            )
            ((odd? count)   
                (fib-iter   (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p 
                            q
                            (- count 1)
                )
            )
    )
)

(define (square x) (* x x))

;笔记里写了这是一个无敌的思路，但是现在我已经想不起来是咋回事了