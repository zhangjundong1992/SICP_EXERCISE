(load "chapter1/sec1.2.scm")

#| 2.2.1 序列的表示 |#
(define nil '())

(define one-through-four (list 1 2 3 4))

(define odds (list 1 3 5 7))

(define squares (list 1 4 9 16 25))

; 读序列的第n项
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

; 求序列的长度
(define (length items)
    (if (null? items)
        0   
        (+ 1 (length (cdr items)))))

; 序列的拼接（追加）
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

; 对表的映射，高阶过程
(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (map proc (cdr items)))))

; 利用map定义scale过程
(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))

#| 2.2.2 层次性结构 |#

; 求树结构的叶子节点数
(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x)) 
                   (count-leaves (cdr x))))))

#| 2.2.3 序列作为界面 |#

; 筛选
(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
; 累积
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
; 枚举
(define (enumerate-int low high)
    (if (> low high)
        nil
        (cons low (enumerate-int (+ low 1) high))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

; 嵌套映射
(define (nest-map n)
    (accumulate append
                nil
                (map (lambda (i) 
                         (map (lambda (j) (list i j))
                              (enumerate-int 1 (- i 1))))
                     (enumerate-int 1 n))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum 
         (filter prime-sum? 
                 (flatmap 
                    (lambda (i) 
                        (map (lambda (j) (list i j))
                             (enumerate-int 1 (- i 1))))
                    (enumerate-int 1 n)))))

