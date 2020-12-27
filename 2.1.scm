; 实现书上的有理数计算
(define (add-rat x y)
    (make-rat   (+  (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (*  (denom x) (denom y))
    )
)

(define (sub-rat x y)
    (make-rat   (-  (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (*  (denom x) (denom y))
    )
)

(define (mul-rat x y)
    (make-rat   (* (numer x) (numer y))
                (* (denom x) (denom y))
    )
)

(define (div-rat x y)
    (make-rat   (* (numer x) (denom y))
                (* (denom x) (numer y))
    )
)

(define (equal-rat? x y)
    (=  (* (numer x) (denom y))
        (* (denom x) (numer y))
    )
)

; 习题2.1
(define (make-rat n d) 
    (let ((g (gcd n d)))
        (if (< d 0)
            (cons (- (/ n g)) (- (/ d g)))
            (cons (/ n g) (/ d g))
        )     
    )   
)

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    ; (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
)

;测试用例
;(print-rat (add-rat (make-rat -1 -3) (make-rat 1 3)))
