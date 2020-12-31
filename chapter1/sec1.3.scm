#| 求和公式的过程抽象 |#
(define (sum term a next b)
    (if (> a b)
        0
        (+  (term a) 
            (sum term (next a) next b))))

(define (sum-int a b)
    (define (identity x) x)
    (define (inc n) (+ n 1))
    (sum identity a inc b))

(define (integral f a b dx)
    (define (add-dx x)  (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))