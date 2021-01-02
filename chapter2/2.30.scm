(load "chapter2/sec2.2.scm")

#| 两种方式实现square-tree |#
; 直接定义
(define (square-tree tree)
    (cond   ((null? tree) '())
            ((not (pair? tree)) (* tree tree))
            (else (cons (square-tree (car tree))
                        (square-tree (cdr tree))))))

; 使用map
(define (square-tree-map tree)
    (map (lambda (sub-tree) 
            (if (pair? sub-tree)
                (square-tree-map sub-tree)
                (* sub-tree sub-tree))) 
         tree))


(define lst 
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6  7)))))))