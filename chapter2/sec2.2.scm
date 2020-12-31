(define one-through-four (list 1 2 3 4))

(define odds (list 1 3 5 7))

(define squares (list 1 4 9 16 25))

#| 读序列的第n项 |#
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

#| 求序列的长度 |#
(define (length items)
    (if (null? items)
        0   
        (+ 1 (length (cdr items)))))

#| 序列的拼接（追加）|#
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

#| 对表的映射，高阶过程 |#
(define (map proc items)
    (if (null? items)
        items
        (cons (proc (car items)) (map proc (cdr items)))))

#| 利用map定义scale过程 |#
(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))