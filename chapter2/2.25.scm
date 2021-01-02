#| 从表中取出7 |#
(define lst-1 (list 1 3 (list 5 7) 9))
(define lst-2 (list (list 7)))
(define lst-3 
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6  7)))))))

(define res-1 (car (cdr (car (cdr (cdr lst-1))))))
(define res-2 (car (car lst-2)))
(define res-3
    (cadadr
    (cadadr
    (cadadr
        lst-3))))
         
; 表3的高阶函数实现
(define (mul-proc proc n items)
    (if (= n 0)
        items
        (mul-proc proc (- n 1) (proc items))))

(define res-3-v2 
    (mul-proc cadr 6 lst-3))
