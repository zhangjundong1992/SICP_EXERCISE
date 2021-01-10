(define (deriv exp var)
    (cond   
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
    (define (deriv-sum operands-part var)
        (make-sum (deriv (car operands-part) var)
                  (deriv (cadr operands-part) var)))
    (put 'deriv '+ deriv-sum)
    'done )

(define (install-product-package)
    (define (deriv-product operands-part var)
        (let ((u (car operands-part))
              (v (cadr operands-part)))
            (make-sum (make-product u (deriv v var))
                      (make-product v (deriv u var)))))
    (put 'deriv '* deriv-product)
    'done )

