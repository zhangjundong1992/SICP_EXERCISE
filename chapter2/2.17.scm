(load "chapter2/sec2.2.scm")

#| 返回list的最后一个序对 |#
(define (last-pair items)
    (if (null? (cdr items))
        items
        (last-pair (cdr items))))

; (last-pair odds)

