#lang racket
;#lang planet neil/sicp

(define one-through-four (list 1 2 3 4))
;one-through-four
;(define squares (list 1 4 9 16 25))
;(define odds (list 1 3 5 7 9))

; cdr down
(define (length items)
  (define (helper count a)
    (if (null? a)
        count
        (helper (+ 1 count) (cdr a))))
  (helper 0 items))

;(length one-through-four)
;(length odds)
;(length squares)

; cons up and cdr down
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;(append '(1 2 3) '(4 5))

; ex2-17
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

;(last-pair '(1 2 3 4 5))
 
; ex2-18
; cons up and cdr down
(define (reverse list)
  (define (helper list rev-list)
    (if (null? list) 
        rev-list
        (helper (cdr list) (cons (car list) rev-list))))
  (helper list '()))

;(reverse '(1))
;(reverse '(1 2))
;(reverse '(1 2 3))

; ex2-19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 .5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
        (+ (cc amount (except-first-denomination coin-values))
           (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (no-more? list)
  (null? list))
(define (first-denomination list)
  (car list))
(define (except-first-denomination list)
  (cdr list))

;(cc 100 us-coins) ; 292

; ex2-20
(define (even? n) (= (remainder n 2) 0))

(define (same-parity n . m)
  (define (filter-iter n list acc)
    (cond ((null? list) acc)
          ((even? (+ n (car list))) (filter-iter n (cdr list) (cons (car list) acc)))
          (else (filter-iter n (cdr list) acc))))
  (reverse (filter-iter n m (list n))))
      
;(same-parity 1 2 3 4 5 6 7 8 9 10)
;(same-parity 2 56 57 58 99 102)
;(same-parity 1)

; ex2-21
(define nil '())
(define (square x) (* x x))
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

;(square-list (list 1 2 3 4 5))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

;(square-list-2 (list -4 5 100 22))

; ex2-22
; the first list item is squared and pushed on a stack
; the last list item squared is at the top of the stack
; the stack is returned and the top of the stack is the last list item squared

; (cons <single value> <list of values>)
; answer is a list of values and cons builds a list of nested lists

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;(square-list-3 (list 1 2 3 4))

; ex2-23
;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))

(define (for-each-4 proc items)
  (if (null? (cdr items))
      (proc (car items))
      (and (proc (car items)) (for-each-4 proc (cdr items)))))

;(for-each-4 (lambda (x) (newline) (display x))
;        (list 57 321 88))
;
;(for-each-4 (lambda (x) (newline)(display x)) (list 1 2 3 4))

; ex2-24
(define xx (cons (list 1 2) (list 3 4)))
;(length x)
(define (count-leaves xx)
  (cond ((null? xx) 0)
        ((not (pair? xx)) 1)
        (else (+ (count-leaves (car xx)) (count-leaves (cdr xx))))))

(display "count-leaves\n")
;(list xx xx)
;(count-leaves (list xx xx))

;(list 1 (list 2 (list 3 4)))

; ex2-25
(define x1 (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr x1)))))

(define x2 '((7)))
;(car (car x2))

(define x3 '(1 (2 (3 (4 (5 (6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x3))))))))))))

; ex2-26
(define x (list 1 2 3))
(define y (list 4 5 6))
;(append x y)
;(cons x y)
;(list x y)

; ex2-27
(define x5 (list (list 1 2) (list 3 4)))
;(reverse x5)

(define (leaf? tree)
  (not (pair? tree)))

(define (tree-manip leaf-op init merge tree)
  (if (null? tree)
      init
      (if (leaf? tree)
          (leaf-op tree)
          (merge (tree-manip leaf-op init merge (car tree))
                 (tree-manip leaf-op init merge (cdr tree))))))


(define (deep-reverse-2 tree)
  (tree-manip (lambda (x) x) nil (lambda (a b) (append b (list a)))tree))

(display "deep-reverse with tree-manip\n")
(display "tree ")
x5
(display "reversed tree ")
(deep-reverse-2 x5)

;(define (deep-reverse items)
(define (deep-reverse list)
  (define (helper list rev-list)
    (cond ((null? list) rev-list)
          ((not (pair? list)) list)
          (else (helper (cdr list) (cons (deep-reverse (car list)) rev-list)))))
  (helper list '()))

;(deep-reverse x5)

; ex2-28
(define x6 (list (list 1 2) (list 3 4)))
(display "2-28 fringe\n")
;x6

(define (fringe atree)
  (define (helper atree leaves)
    (cond ((null? atree) leaves)
          ((not (pair? atree)) (cons atree leaves))
          (else (helper (car atree) (append (fringe (cdr atree)) leaves)))))
  (helper atree nil))

;(fringe x6) 
;(fringe (list x6 x6))  

; ex2-29
(display "2-29 mobile problem \n")

(display "constructors and selectors\n")
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (left-branch mobile)
  (car mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(display "total weight\n")
(define (total-weight mobile)
  (define (helper mobile total)
    (cond ((null? mobile) total)
          (else (let ((lm (branch-structure (left-branch mobile)))
                (rm (branch-structure (right-branch mobile))))
            (cond ((and (pair? lm) (pair? rm))
                (+ (helper lm total) (helper rm total)))
                  ((pair? lm) (+ (helper lm total) rm))
                  ((pair? rm) (+ (helper rm total) lm))
                  (else (+ lm rm total)))))))
  (helper mobile 0))

(define mymobile (make-mobile (make-branch 5 5) (make-branch 8 (make-mobile (make-branch 3 4) (make-branch 5 6)))))
(define mymobile2 (make-mobile (make-branch 6 6) (make-branch 6 6)))
(define mymobile3 (make-mobile (make-branch 8 (make-mobile (make-branch 4 5) (make-branch 5 4))) (make-branch 9 8)))
(display "mymobile\n")
;mymobile
(display "mymobil2e\n")
;mymobile2
(display "mymobile3\n")
;mymobile3

;(left-branch mymobile)
;(branch-structure (right-branch mymobile))
;(right-branch mymobile)
;(total-weight mymobile)

(define (is-balanced mobile)
    (cond ((null? mobile) #t)
          (else (let ((ls (branch-structure (left-branch mobile)))
                      (llen (branch-length (left-branch mobile)))
                      (rs (branch-structure (right-branch mobile)))
                      (rlen (branch-length (right-branch mobile))))
            (cond ((and (pair? ls) (pair? rs))
                (and (is-balanced ls) (is-balanced rs)))
                  ((pair? ls) (and (= (* rlen rs) (* llen (total-weight ls)))))
                  ((pair? rs) (and (= (* llen ls) (* rlen (total-weight rs)))))
                  (else (= (* rlen rs) (* llen ls))))))))

(display "is-balanced\n")
;(is-balanced mymobile)
;(is-balanced mymobile2)
;(is-balanced mymobile3)
;(right-branch mymobile3)
;(left-branch mymobile3)
;(branch-length (left-branch mymobile3))
;(branch-structure (left-branch mymobile3))
;(left-branch (branch-structure (left-branch mymobile3)))
;(branch-structure (left-branch (branch-structure (left-branch mymobile3))))

;(is-balanced mymobile3)
;(is-balanced (make-mobile (make-branch 6 6) (make-branch 6 3)))

; selectors change; that's it!

; ex2-30 warm-up: scale-tree 

(display "scale tree with recursion\n")
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(display "scale tree with map and recursion\n")
(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-2 sub-tree factor)
             (* sub-tree factor)))
       tree))

; ex2-30 square-tree
(display "ex2-30\n")

(define atree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;atree

(display "without map\n")
(define (square-tree-1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-1 (car tree)) (square-tree-1 (cdr tree))))))

;(square-tree-1 atree)

(display "with map\n")
(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (* sub-tree sub-tree)))
       tree))
;(square-tree-2 atree)

; ex2-31
(display "ex 2-31: tree-map\n")
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
 

(define (square-tree tree)
  (tree-map square tree))

(display "tree and square-tree\n")
;atree
;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))   
  
; ex 2-32
(display "2-32: subsets\n")

;(subsets (list 1 2 3))
;(define rest (list nil '(3) '(2) '(2 3)))
;rest
;(map (lambda (x) (append '(1) x)) rest)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

;(subsets '(1 2 3))
;(subsets '(1 2 3 4))

; ex 2-33 complete the following definitions of some basic list manipulation operations as accumulations:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(display "define map\n")
(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map-2 square (list -1 -2 3 -4))

(display "define append\n")
(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))

(append-2 (list 1 2 3) (list 4 5 6))

(display "define len sequence\n")
(define (length-2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length-2 (list 1 2 3 4))

; ex 2-34 Horner's Rule
(display "Horner's Rule\n")
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))

; 2-35 redefine count-leaves as an accumulation
(display "redefine count leaves\n")
(define (count-leaves-2 t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
        
;(count-leaves-2 (list xx xx))

; ex 2-36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; ex 2-37
(define v1 (list 1 2 3 4))
(define m1 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define m2 (list (list 1 2) (list 3 4)))
(define m2id (list (list 1 0) (list 0 1)))
(define mid (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

(display "dot product\n")
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(display "matrix-*-vector\n")
(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (accumulate-n * 1 (list x v)))) m))
  ;(map (lambda (y) (dot-product v y)) m))

;m2
(define v2 (list 3 3))
;v2
;(matrix-*-vector m2 v2)
;m1
;v1
;(matrix-*-vector m1 v1)

(display "transpose\n")
(define (transpose mat)
  (accumulate-n cons '() mat))
;m1
(transpose m1)
;(transpose mid)

(display "matrix-*-matrix\n")
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define m13 (list (list 10 10 10)))
;m13
;m1
;(matrix-*-matrix m13 m1)
(define n (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define cols (transpose n))
;m2
;m2
;(matrix-*-matrix m2 m2)

; ex 2-38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
;(fold-right / 1 (list 1 2 3)) ; 3/2
;(fold-left / 1 (list 1 2 3)) ; 1/6
;(fold-right list nil (list 1 2 3)) ; '(1 (2 (3 ())))
;(fold-left list nil (list 1 2 3)) ; '(((() 1) 2) 3)

; op should be commutative

;ex  2-39
(display "reverse using fold-right\n")
(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;(reverse-1 (list 1 2 3 4 5))

(display "reverse using fold-left\n")
(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;(reverse-2 (list 1 2 3))

; ex 2-40
(display "unique-pairs\n")

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (myproc i)
  (map (lambda (j) (list i j))
       (enumerate-interval 1 (- i 1))))
;
(define (myproc-triple i)
  (flatmap 
   (lambda (j)
          (map (lambda (k) 
                 (list i j k))
              (enumerate-interval 1 (- j 1))))
         (enumerate-interval 2 (- i 1))))

(display "flatmap experiments\n")
(define (unique-triples-2 n)
  (flatmap myproc-triple (enumerate-interval 1 n)))
;(unique-triples-2 5)

(define (unique-pairs n)
  (flatmap myproc (enumerate-interval 1 n)))

;(unique-pairs 5) 
 
; (display "prime-sum-pairs\n")

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;(prime-sum-pairs 6)

; ex 2-41
(define (myproc-triple-2 n)
         (map (lambda (j) (append (list n) j))
              (unique-pairs (- n 1))))

(define (unique-triples n)
  (flatmap myproc-triple-2 (enumerate-interval 1 n)))

;(define (equal-s? s)
;  (lambda (triple)
;    (= (+ (car triple) (cadr triple) (cadr (cdr triple))) s)))
;;
;(define (make-triple-sum x)
;  (list (+ (car x) (cadr x) (cadr (cdr x)))))
;;
;(define (make-sum-triples n s)
;    (filter (equal-s? s)
;        (unique-triples n)))

(display "unique-triples\n")
;(unique-triples 5)
;(filter (equal-s? 9) (unique-triples 5))
;(make-sum-triples 10 16)

; ex 2-42
(define empty-board nil)

(define (position row col)
  (list row col))

(define (row position)
  (car position))

(define (col position)
  (cadr position))

(define (adjoin-position new-row k rest-of-queens)
  (cons (position new-row k) rest-of-queens))

(define (on-diagonal? pos-1 pos-2)
  (= (abs (- (col pos-1) (col pos-2))) (abs (- (row pos-1) (row pos-2)))))

(define (same-row? pos-1 pos-2)
  (= (row pos-1) (row pos-2)))

(define (safe? k positions)
  (let ((pos-k (car positions)))
    (fold-left (lambda (a b) (and a b)) #t
               (map (lambda (p) (not (or (same-row? pos-k p) (on-diagonal? pos-k p)))) (cdr positions)))))
  
;(define (safe? k positions)
;  (let ((pos-k (car positions)))
;  (fold-left (lambda (a b) (and a b)) #t
;   (map (lambda (p) (not (or (= (row pos-k) (row p)) (= (abs (- k (col p))) (abs (- (row pos-k) (row p))))))) (cdr positions)))))
;
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        ;(filter
         ;(lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 6)))
          (queen-cols (- k 1)))));)

(define answer (queens 8))
(queens 8)
(length answer)

;(define k 2)  
;(map (lambda (positions)
;       (safe? k positions))
;     (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens))
;                 (enumerate-interval 1 6)))
;          (queen-cols (- k 1))))
