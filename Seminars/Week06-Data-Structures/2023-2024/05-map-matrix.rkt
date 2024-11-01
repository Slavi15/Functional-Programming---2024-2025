#lang racket

(define (map-matrix f m) (map (lambda (row) (map f row)) m))

(map-matrix (lambda (x) (* x x)) '((1 2 3) (4 5 6) (7 8 9)))