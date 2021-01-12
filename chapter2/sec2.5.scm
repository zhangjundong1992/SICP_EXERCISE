#| 2.5.1 通用型算术运算 |#
; H1
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 分派器
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method of these types --APPLT-GENETIC"
                       (list op type-tags))))))

; H2-scheme-number
(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ (cdr x) (cdr y)))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- (cdr x) (cdr y)))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* (cdr x) (cdr y)))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ (cdr x) (cdr y)))))
    (put 'make 'scheme-number (lambda (x) (tag x)))
    'done )

(define (make-scheme-number n)
    ((get 'make 'scheme-number ) n))

#| 2.5.3 多项式算术 |#
; H2-poly
(define (install-poly-package)
    ; poly
    (define (make-poly variable term-list) (cons varialbe term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (=zero-poly? poly)
        (apply and (map =zero? (map coeff (term-list poly)))))
    ; variable
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))
    ; term-list
    (define (the-empty-termlist)'())
    (define (adjoin-terms term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (empty-termlist? term-list) (null? term-list))
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    ; term，注意使用的是list，而不是cons
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))
    ; operation
    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else (let ((t1 (first-term L1)) (t2 (first-term L2)))
                        (cond ((> (order t1) (order t2)) 
                                    (adjoin-terms t1 (add-terms (rest-terms L1) L2)))
                              ((< (order t1) (order t2))
                                    (adjoin-terms t2 (add-terms L1 (rest-terms L2))))
                              (else (adjoin-terms (make-term (order t1) (add (coeff t1) (coeff t2)))
                                            (add-terms (rest-terms L1) (rest-terms L2)))))))))
    (define (mul-terms L1 l2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                       (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-terms (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                        (mul-term-by-all-terms t1 (rest-terms L))))))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var --ADD-POLY" (list p1 p2))))
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var --MUL-POLY" (list p1 p2))))
    ; interface
    (define (tag p) (attach-tag 'poly p))
    (put 'add '(poly poly) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(poly poly) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'poly (lambda (var terms) (tag (make-poly var terms))))
    (put '=zero? 'poly =zero-poly?)
    'done )

(define (make-poly var terms)
    (get 'make 'poly) var terms))
