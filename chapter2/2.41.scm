(load "chapter2/sec2.2.scm")

(define (sum-trip n s)
    (define (sum-equ? seq)
        (= s (+ (car seq) (cadr seq) (caddr seq))))

    (define (trip n)
        (flatmap    
            (lambda (i)
                (flatmap 
                    (lambda (j) 
                        (map (lambda (k) (list k j i))
                             (enumerate-int 1 (- j 1))))
                    (enumerate-int 1 (- i 1))))
            (enumerate-int 1 n)))

    (filter sum-equ? (trip n)))