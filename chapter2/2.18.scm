(load "chapter2/sec2.2.scm")

#| 序列的逆置，迭代计算过程 |#
(define (reverse items)
    (define (reverse-iter src des)
        (if (null? src)
            des
            (reverse-iter (cdr src) (cons (car src) des))))
    (reverse-iter items '()))

#| 递归计算过程 |#
(define (reverse-recur items)
    (if (null? items)
        items
        (append (reverse-recur (cdr items)) (cons (car items) '()))))

; (reverse odds) (reverse-iter odds)
#| 实现很完美，还是有点小确幸的，鲑鱼鲑鱼！|#