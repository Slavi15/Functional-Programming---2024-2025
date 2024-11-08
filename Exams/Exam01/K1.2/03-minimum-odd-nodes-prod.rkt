#lang racket

(define // quotient)
(define % modulo)

(define (make-tree root left right) (list root left right))
(define mk make-tree)

(define (make-leaf root) (list root '() '()))

(define (root tr) (car tr))
(define (left-t tr) (cadr tr))
(define (right-t tr) (caddr tr))

(define (empty? tr) (null? tr))

(define (leaf? tr) (and (empty? (left-t tr)) (empty? (right-t tr))))

(define (divides n k) (zero? (% n k)))

(define (odd? n) (not (divides n 2)))

(define (minimum-odd-nodes-prod bt)
  (define (get-paths tr path product)
    (cond
      [(empty? tr) (list (cons path product))]
      [(odd? (root tr))
       (let* ([new-product (* product (root tr))]
             [left-result (get-paths (left-t tr) (cons (root tr) path) new-product)]
             [right-result (get-paths (right-t tr) (cons (root tr) path) new-product)])
         (append left-result right-result)
         )
       ]
      [else
       (let ([left-result (get-paths (left-t tr) (cons (root tr) path) product)]
             [right-result (get-paths (right-t tr) (cons (root tr) path) product)])
         (append left-result right-result)
         )
       ]
      )
    )

  (define (find-min-path paths)
    (define (compare p1 p2)
      (cond
        [(> (cdr p1) (cdr p2)) p2]
        [(< (cdr p1) (cdr p2)) p1]
        [else (if (> (length (car p1)) (length (car p2))) p1 p2)]
        )
      )

    (foldr compare (car paths) (cdr paths))
    )

  (reverse (car (find-min-path (get-paths bt '() 1))))
  )

(minimum-odd-nodes-prod '(1 (3 () ()) (-3 (4 () ()) (-1 () ()))))
(minimum-odd-nodes-prod '(1 (3 () ()) (-3 (-5 () ()) (-1 () ()))))