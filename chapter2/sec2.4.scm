#| 抽象数据的多重表示 |#

#| 数据导向程序设计 |#
; H1
(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
                    
(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

; H2
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular ) x y))
(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar ) r a))

; H3
(define (install-rectangular-package)
    ; 内部过程
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))
    
    ; 调用接口
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular 
        (lambda (r a) (tag (make-from-imag-mag r a))))
    'done )

(define (install-polar-package) 'done )

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum --TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum --CONTENTS" datum)))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method of these types --APPLT-GENETIC" 
                        (list op type-tags))))))

#| 消息传递 |#

; H2
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond ((eq? op 'real-part ) x)
            ((eq? op 'imag-part ) y)
            ((eq? op 'magnitude ) (sqrt (+ (square x) (square y))))
            ((eq? op 'angle ) (atan y x))
            (else (error "Unknow op --MAKE-FROM-REAL-IMAG" op))))
    dispatch)

(define (apply-genetic op arg)
    (arg op))