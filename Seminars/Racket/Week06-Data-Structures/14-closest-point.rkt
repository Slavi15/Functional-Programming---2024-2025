#lang racket

(define (sq x) (* x x))

(define (distance p1 p2)
  (sqrt (+
         (sq (- (car p1) (car p2)))
         (sq (- (cdr p1) (cdr p2)))))
  )

(define (closest-point xys)
  (define (iter xs min-dist min-point point)
    (cond
      [(null? xs) min-point]
      [(< (distance (car xs) point) min-dist) (iter (cdr xs) (distance (car xs) point) (car xs) point)]
      [else (iter (cdr xs) min-dist min-point point)])
    )
  
  (lambda (point)
    (iter (cdr xys) (distance (car xys) point) (car xys) point)
    )
  )

((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(2 . 1))   ;; => '(0 . 0)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(8 . 9))   ;; => '(10 . 10)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(10 . 1))  ;; => '(10 . 0)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(1 . 10))  ;; => '(0 . 10)