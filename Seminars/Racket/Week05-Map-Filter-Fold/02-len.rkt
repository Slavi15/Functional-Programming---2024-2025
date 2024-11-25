#lang racket

(define (len l) (foldr (lambda (_ xs) (+ 1 xs)) 0 l))

(len '())      ;; => 0
(len '(123))   ;; => 1
(len '(1 2 3)) ;; => 3