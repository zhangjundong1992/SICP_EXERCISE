#| 规范化正数负数显示 |#
(define (make-rat n d) 
    (let ((g (gcd n d)))
        (if (< d 0)
            (cons (- (/ n g)) (- (/ d g)))
            (cons (/ n g) (/ d g)))))

;(print-rat (add-rat (make-rat -1 -3) (make-rat 1 3)))
