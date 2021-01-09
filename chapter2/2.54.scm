#| 没有加入空表的判断 |#
(define (equal? seq1 seq2)
    (cond   ((and (symbol? seq1) (symbol? seq2))
                (eq? seq1 seq2))
            ((and (list? seq1) (list? seq2)) 
                (and (equal? (car seq1) (car seq2))
                     (equal? (cdr seq1) (cdr seq2))))
            (else #f)))