(load "chapter2/sec2.2.scm")

(define (for-each proc items)
    (if (not (null? items))
        (begin  (proc (car items)) 
                (for-each proc (cdr items)))))

(define test
    (for-each (lambda (x) (newline) (display x)) odds))