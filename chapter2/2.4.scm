#| cons的过程性表示，很强！！！|#
(define (cons x y)
    (lambda (m) (m x y))) #| 返回值是一个过程，参数m也是一个过程 |#

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))