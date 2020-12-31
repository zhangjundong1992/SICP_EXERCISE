(load "chapter1/sec1.2.scm")

#| 用加法模拟乘法，递归计算过程 |#
(define (mul a b)
    (cond   ((= b 0) 0)
            ((even? b) (double (mul a (halve b))))
            ((odd? b) (+ a (mul a (- b 1))))))

   