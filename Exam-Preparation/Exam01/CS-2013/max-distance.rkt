#lang racket

(define (sq x) (* x x))

(define (distance p1 p2)
  (let ([x1 (car p1)]
        [x2 (car p2)]
        [y1 (cadr p1)]
        [y2 (cadr p2)])
    (sqrt (+ (sq (- x1 x2)) (sq (- y1 y2))))
    )
  )

(define (max-distance l)
  (define (iter max-dist lst)
    (if (null? lst)
        max-dist
        (let loop ([p1 (car lst)]
                   [rest (cdr lst)]
                   [current-max max-dist])
          (if (null? rest)
              (iter current-max (cdr lst))
              (let ([p2 (car rest)])
                (loop p1 (cdr rest) (max current-max (distance p1 p2)))
                )
              )
          )
        )
    )
  (iter 0 l)
  )

(define points `((-1.1 1) (1.8 2) (3 1) (-1 -2)))
(max-distance points)