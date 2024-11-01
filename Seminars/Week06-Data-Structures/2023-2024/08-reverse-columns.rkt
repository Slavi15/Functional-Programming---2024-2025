#lang racket

(define (reverse-columns m) (map (lambda (row) (reverse row)) m))

(reverse-columns '((1 2 3) (4 5 6) (7 8 9))) ;; => '((3 2 1) (6 5 4) (9 8 7))