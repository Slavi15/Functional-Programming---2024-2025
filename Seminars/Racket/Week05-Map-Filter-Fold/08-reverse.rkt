#lang racket

(define (reverse l) (foldl cons `() l))

(reverse '(1 2 3)) ;; => '(3 2 1)