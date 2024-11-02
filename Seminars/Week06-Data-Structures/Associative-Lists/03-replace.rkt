#lang racket

(define (replace xs dict)
  (cond
    [(null? dict) xs]
    [(null? xs) `()]
    [(equal? (car xs) (caar dict)) (cons (cdar dict) (replace (cdr xs) (cdr dict)))]
    [else (cons (car xs) (replace (cdr xs) dict))])
  )

(replace '(1 2 3 4) '((1 . 11) (2 . 22)))
(equal? (replace '(1 2 3 4) '((1 . 11) (2 . 22))) '(11 22 3 4))