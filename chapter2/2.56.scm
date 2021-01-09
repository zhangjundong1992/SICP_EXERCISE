(load "chapter2/sec2.3.scm")
; 求导过程
(define (deriv exp var)
    (cond   
        ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
        ((product? exp)
            (make-sum (make-product (multiplier exp)
                                    (deriv (multiplicand exp) var))
                      (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp))))
        ((exponentiation? exp) 
            (make-product 
                          (make-product (exponent exp) 
                                        (make-exponentiation (base exp) (- (exponent exp) 1)))
                          (deriv (base exp) var)))
        (else (error "unknown exp" exp))))


; 幂式定义
(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation u n)
    (cond ((=number? n 0) 1)
          ((=number? n 1) u)
          (else (list '** u n))))

(define (base e)
    (cadr e))

(define (exponent e)
    (caddr e))

; 并不完美，没有实现n是变量的情况