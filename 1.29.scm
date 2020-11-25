;求和公式的过程抽象
(define (sum term a next b)
    (if (> a b)
        0
        (+  (term a) 
            (sum term (next a) next b)
        )    
    )
)

(define (sum-int a b)
    (define (identity x) x)
    (define (inc n) (+ n 1))
    (sum identity a inc b)
)

(define (integral f a b dx)
    (define (add-dx x)  (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

(define (cube x) (* x x x))

;辛普森算法求解积分
(define (sum-simp term a b k n)
    (if (> k n)
        0
        (+  (term a b k n) 
            (sum-simp term a b (+ k 1) n)
        )    
    )
)

(define (integral-simp f a b n)
    (define (f-simp a b k n)
        (let ((v-iter (+ a (* k (/ (- b a) n)))))
            (cond ((or (= k 0) (= k n)) (f v-iter))
              ((odd? k) (* 4 (f v-iter) ))
              ((even? k) (* 2 (f v-iter)))
            )
        )    
    )

    (* (/ (- b a) n 3)
       (sum-simp f-simp a b 0 n) 
    )
)

;(integral-simp cube 0 1 100)