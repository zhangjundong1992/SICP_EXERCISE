(load "chapter1/sec1.2.scm")

#| 用加法模拟乘法，迭代计算过程 |#
(define (mul a b)
    (mul-iter a b 0)
)

(define (mul-iter a b res)
    (cond   ((= b 0) res)
            ((even? b) (mul-iter (double a) (halve b) res))
            ((odd? b) (mul-iter a (- b 1) (+ res a)))))

#| 各单位注意！这中思路可以转换为通用思路，实现对数规模的计算 |#