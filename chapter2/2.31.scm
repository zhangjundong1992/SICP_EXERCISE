#| tree-map实现 |#
(define (tree-map proc tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (proc tree))
          (else (cons (tree-map proc (car tree)) 
                      (tree-map proc (cdr tree))))))

(define (square-tree tree)
    (tree-map (lambda (x) (* x x)) tree))


(define lst
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6  7)))))))