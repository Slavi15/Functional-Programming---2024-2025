#lang racket

(define (replace lst dict)
  (foldr append `()
         (map
          (lambda (x) (if (assoc x dict) (list (cdr (assoc x dict))) (list x)))
          lst)
         )
  )

(replace '(1 2 3 4) '((1 . c) (4 . 1)))  ;; => '(c 2 3 1)