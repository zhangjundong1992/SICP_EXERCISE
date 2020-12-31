(load "chapter1/sec1.2.scm")

#| 快速幂算法，迭代计算过程联系 |#
(define (fast-expt b n)
    (expt-iter b n 1))

(define (expt-iter b n a)
    (cond   ((= n 0) a)
            ((even? n) (expt-iter (square b) (/ n 2) a))
            ((odd? n) (expt-iter b (- n 1) (* b a)))))

