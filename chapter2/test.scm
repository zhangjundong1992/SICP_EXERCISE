#| 带点尾部记法 |#
; (define (myprint .param)
; ;   (display string)
; ;   (newline)
;   (for-each (lambda (x) (display x) (newline)) param)
; )
; ; (myprint "begin print" 2 3 4 5)

#| applt函数 |#
; (define sum (lambda args (apply + args)))
; (define sum +)
; (define test list)