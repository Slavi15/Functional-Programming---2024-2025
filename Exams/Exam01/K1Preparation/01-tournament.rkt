#lang racket

(define (alist-operation alist key value op)
  (map
   (lambda (x)
     (if (equal? key (car x))
         (let ([val (cdr (assoc (car x) alist))])
           (cons key (op val value))
           )
         x
         )
     )
   alist)
  )

(define (add-to-list alist key value op)
  (if (assoc key alist)
      (alist-operation alist key value op)
      (cons (cons key value) alist)
      )
  )

(define (get-points alist)
  (foldr
   (lambda (x res)
     (let ([key1 (car x)]
           [key2 (cadr x)]
           [val1 (caddr x)]
           [val2 (cadddr x)])
       (cond
         [(= val1 val2) (add-to-list (add-to-list res key2 1 +) key1 1 +)]
         [(> val1 val2) (add-to-list res key1 3 +)]
         [else (add-to-list res key2 3 +)]
         )
       )
     )
   '()
   alist)
  )

(define (get-goals-made alist)
  (foldr
   (lambda (x res)
     (let ([key1 (car x)]
           [key2 (cadr x)]
           [val1 (caddr x)]
           [val2 (cadddr x)])
       (add-to-list (add-to-list res key2 val2 +) key1 val1 +)
       )
     )
   '()
   alist)
  )

(define (get-goals-taken alist)
  (foldr
   (lambda (x res)
     (let ([key1 (car x)]
           [key2 (cadr x)]
           [val1 (caddr x)]
           [val2 (cadddr x)])
       (add-to-list (add-to-list res key2 val1 +) key1 val2 +)
       )
     )
   '()
   alist)
  )

(define (max-goal-diff win-list lost-list)
  (foldr
   (lambda (x acc)
     (max acc (- (cdr x) (cdr (assoc (car x) lost-list))))
     )
   (- (cdar win-list) (cdar lost-list))
   win-list)
  )

(define (filter-max-diff win-list lost-list diff)
  (filter
   (lambda (x) (= diff (- (cdr x) (cdr (assoc (car x) lost-list)))))
   win-list)
  )

(define (max-goal-min-points alist)
  (let* ([pts (get-points alist)]
         [goals-made (get-goals-made alist)]
         [goals-taken (get-goals-taken alist)]
         [max-diff (max-goal-diff goals-made goals-taken)]
         [filtered-max (map car (filter-max-diff goals-made goals-taken max-diff))]
         [filtered-points (filter (lambda (x) (member (car x) filtered-max)) pts)])
    (car (foldr
          (lambda (x res)
            (if (< (cdr x) (cdr res)) x res)
            )
          (car filtered-points)
          filtered-points))
    )
  )

(max-goal-min-points `(("A" "B" 1 0) ("B" "C" 4 1) ("C" "B" 3 3) ("B" "A" 1 2) ("A" "C" 0 1)))