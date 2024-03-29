(define (square x)
    (* x x))

(define (double x)
    (+ x x))

(define (halve x)
    (/ x 2)) 

(define (divides? a b)
    (= (remainder b a) 0))

#| 1.2.6 素性检测 |#

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
    (if (< n 2)
        #f
        (= n (smallest-divisor n))))