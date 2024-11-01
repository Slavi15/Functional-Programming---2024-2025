#lang racket

(define (make-tree tr left right) (list tr left right))
(define mk make-tree)

(define (make-leaf tr) (list tr `() `()))

(define (leaf? tr) (and
                    (not (empty-tree? tr))
                    (empty-tree? (left-tree tr))
                    (empty-tree? (right-tree tr))))

(define (empty-tree? tr) (null? tr))

(define (root tr) (car tr))
(define (left-tree tr) (cadr tr))
(define (right-tree tr) (caddr tr))

;; I have made all these tasks by myself :)

;; ---------------- Trees ------------------

(define my-tree
  (make-tree 1
             (make-tree 2
                        (make-leaf 4)
                        (make-leaf 5)
                        )
             (make-leaf 3)
             )
  )

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define bst (make-tree 3
                       (make-tree 1
                                  '()
                                  (make-leaf 2))
                       (make-tree 4
                                  '()
                                  (make-leaf 5))))

(define bst-tree
  (mk 8
      (mk 3
          (make-leaf 1)
          (mk 6
              (make-leaf 4)
              (make-leaf 7)
              )
          )
      (mk 10
          (list)
          (mk 14
              (make-leaf 13)
              (list)
              )
          )
      )
  )

(define balanced-tree
  '(4
    (2
     (1 () ())
     (3 () ()))
    (6
     (5 () ())
     (7 () ())))
  )

;; ---------------- Trees ------------------

(define (pre-order t)
  (if (empty-tree? t)
      (list)
      (append (list t) (left-tree t) (right-tree t))
      )
  )

(define (in-order t)
  (if (empty-tree? t)
      (list)
      (append (left-tree t) (list t) (right-tree t))
      )
  )

(define (post-order t)
  (if (empty-tree? t)
      (list)
      (append (left-tree t) (right-tree t) (list t))
      )
  )


(define (map-tree f t)
  (if (empty-tree? t)
      (list)
      (let ([value (root t)]
            [left (left-tree t)]
            [right (right-tree t)])
        (mk (f value) (map-tree f left) (map-tree f right))
        )
      )
  )

(define (filter-tree f t)
    (cond
      [(empty-tree? t) `()]
      [(leaf? t) (if (f (root t)) (list (root t)) `())]
      [else (append (filter-tree f (left-tree t)) (filter-tree f (right-tree t)))])
  )

(define (size t)
  (if (empty-tree? t)
      0
      (+ 1 (size (left-tree t)) (size (right-tree t)))
      )
  )

(define (height t)
  (if (empty-tree? t)
      0
      (+ 1
         (max (height (left-tree t)) (height (right-tree t)))
         )
      )
  )

(define (level n t)
  (define (iter i lst t)
    (cond
      [(empty-tree? t) `()]
      [(zero? i) (cons (root t) lst)]
      [else (append (iter (- i 1) lst (left-tree t)) (iter (- i 1) lst (right-tree t)))])
    )
  (iter n `() t)
  )

(define (count-leaves t)
  (if (leaf? t)
      1
      (+ (count-leaves (left-tree t))
         (count-leaves (right-tree t)))
      )
  )

(define (remove-leaves t)
  (if (leaf? t)
      `()
      (let ([value (root t)]
            [left (left-tree t)]
            [right (right-tree t)])
        (mk value (remove-leaves left) (remove-leaves right))
        )
      )
  )

(define (invert t)
  (if (empty-tree? t)
      `()
      (let ([value (root t)]
            [left (left-tree t)]
            [right (right-tree t)])
        (mk value (invert right) (invert left))
        )
      )
  )

(define (bst? t)
  (if (leaf? t)
      #t
      (let ([value (root t)]
            [left (left-tree t)]
            [right (right-tree t)])
        (and
         (if (leaf? left)
             #t
             (< (root left) value)
             )
         (if (leaf? right)
             #t
             (> value (root right))
             )
         (and (bst? left) (bst? right))
         )
        )
      )
  )

(define (bst-insert x t)
  (cond
    [(empty-tree? t) (make-leaf x)]
    [(> x (root t)) (mk (root t) (left-tree t) (bst-insert x (right-tree t)))]
    [else (mk (root t) (bst-insert x (left-tree t)) (right-tree t))])
  )

(define (bst-remove x t)
  (define (find-min tr)
    (if (empty-tree? (left-tree tr))
        (root tr)
        (find-min (left-tree tr))
        )
    )

  (define (remove-root tr)
    (if (empty-tree? (right-tree tr))
        (left-tree tr)
        (let ([next (find-min (right-tree tr))])
          (mk next (left-tree tr) (bst-remove next (right-tree tr)))
          )
        )
    )
  
  (let ([value (root t)]
        [left (left-tree t)]
        [right (right-tree t)])
    (cond
      [(= x value) (remove-root t)]
      [(> x value) (mk value left (bst-remove x right))]
      [else (mk value (bst-remove x left) right)])
    )
  )

(define (tree-to-list t)
  (cond
    [(empty-tree? t) `()]
    [(leaf? t) (list (root t))]
    [else (append (tree-to-list (left-tree t))
                  (list (root t))
                  (tree-to-list (right-tree t)))])
  )

(define (tree-sort t) (tree-to-list (foldr bst-insert `() t)))

(define (balanced? t)
  (if (empty-tree? t)
      #t
      (let* ([left (left-tree t)]
             [right (right-tree t)]
             [diff (abs (- (height left) (height right)))])
        (and
         (or (= diff 0) (= diff 1))
         (balanced? left)
         (balanced? right)
         )
        )
      )
  )

;; -------------------------------------------------------------------------
;; Inputs

(pre-order my-tree)
(in-order my-tree)
(post-order my-tree)

(newline)

my-tree
(map-tree (lambda (x) (* x x)) my-tree)
(filter-tree even? our-cool-tree) ;; => '(2 4)

(newline)

(size my-tree)
(height my-tree)
(level 1 my-tree)
(count-leaves my-tree)
(remove-leaves my-tree)
(invert my-tree)

(newline)

(bst-insert 2 bst-tree)
(bst-remove 3 bst) ; => '(4 (1 () (2 () ())) (5 () ()))
(tree-sort '(5 1 4 6 3 7)) ; => '(1 3 4 5 6 7)

(newline)

(balanced? balanced-tree)