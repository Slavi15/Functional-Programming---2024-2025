#lang racket

(define 1+ (lambda (x) (+ 1 x)))

(define (at m i)
  (cond
    [(null? m) `()]
    [(zero? i) (car m)]
    [else (at (cdr m) (- i 1))])
  )

(define (main-diagonal m)
  (define (iter i mtx)
    (if (null? mtx)
        `()
        (cons (at (at m i) i) (iter (1+ i) (cdr mtx)))
        )
    )
  (iter 0 m)
  )

(main-diagonal '((1 2 3) (4 5 6))) ;; => '(1 5)