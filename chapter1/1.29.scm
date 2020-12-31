#| 辛普森算法求解积分 |#
(define (sum-simp term a b k n)
    (if (> k n)
        0
        (+  (term a b k n) 
            (sum-simp term a b (+ k 1) n))))

(define (integral-simp f a b n)
    (define (f-simp a b k n)
        (let ((v-iter (+ a (* k (/ (- b a) n)))))
            (cond ((or (= k 0) (= k n)) (f v-iter))
              ((odd? k) (* 4 (f v-iter) ))
              ((even? k) (* 2 (f v-iter))))))

    (* (/ (- b a) n 3)
       (sum-simp f-simp a b 0 n)))

;(integral-simp cube 0 1 100)