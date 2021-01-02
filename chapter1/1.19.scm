(load "chapter1/sec1.2.scm")

#| 迭代计算过程求解斐波那契数列 |#
(define (fib n)
    (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
    (cond   ((= count 0) b)
            ((even? count) 
                (fib-iter   a
                            b
                            (+ (square p) (square q))
                            (+ (* 2 p q) (square q))
                            (/ count 2)))
            ((odd? count)  
                (fib-iter   (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))

#| 对数规模求解斐波那契数列的算法，与快速幂的思想类似（与利用加法模拟乘法的思路类似），利用Tpq变换的性质，见课本31页 |#