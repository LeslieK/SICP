#lang racket
(define nil '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ex 2.59. set represenation: unordered list w/o duplicates

(define (union-set s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
          (else (cons (car s1) (union-set (cdr s1) s2)))))
        
         
(define s1 '(1 2 3 4))
(define s2 '(5 6 7 8))
;(union-set s1 s2)
;(union-set '() s2)

; ex 2.60 set representation: unordered list w duplicates

; element-of-set? => no change

(define (adjoin-set-d x set)
  (cons x set))

(define (union-set-d s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((adjoin-set-d (car s1) (union-set-d (cdr s1) s2)))))

; intersection-of-set? => no change

(define ss1 '( 1 1 2 3))
(define ss2 '(5 6 7 3 3 4))
(define ss3 '())
;(adjoin-set-d '9 ss1)
;(union-set-d ss1 ss2)
;(intersection-set ss1 ss2)

; ex. 2.61 set representation: sorted list; w/o duplicates

(define (element-of-set?-s x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?-s x (cdr set)))))

(define (intersection-set-s set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-s (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-s (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-s set1 (cdr set2)))))))

(define (adjoin-set-s x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (adjoin-set-s x (cdr set)))))

;(adjoin-set-s 1 s2) 

; Exercise 2.62. Give a O(n) implementation of union-set for sets represented as ordered lists.

(define (union-set-s set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
         (let ((x (car set1)))
           (cond ((= x (car set2)) (cons x (union-set-s (cdr set1) (cdr set2))))
                 ((< x (car set2)) (cons x (union-set-s (cdr set1) set2)))
                 (else (cons (car set2) (union-set-s set1 (cdr set2)))))))))

;(union-set-s '(2 3 5 7 9) '(0 2 5 23 25))

; set represented as a binary tree

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set?-t x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set?-t x (left-branch set)))
        (else (element-of-set?-t x (right-branch set)))))


(define (adjoin-set-t x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set-t x (left-branch set)) (right-branch set)))
        (else (make-tree (entry set) (left-branch set) (adjoin-set-t x (right-branch set))))))

; transform tree to a list
(define (tree->list1 tree)
  (cond ((null? tree) '())
        (else
         ;(display "tree is ") (display tree) (newline)
         (append (tree->list1 (left-branch tree)) (cons (entry tree) (tree->list1 (right-branch tree)))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (cond ((null? tree) result-list)
          (else 
           ;(display "tree is ") (display tree) (newline)
           (copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list (right-branch tree) result-list))))))
  (copy-to-list tree '()))

; ex 2.63 part a: Do the two procedures produce the same result for every tree? yes
(define ta (make-tree 27 (make-tree 3 (make-tree 1 nil nil) (make-tree 26 nil nil)) (make-tree 30 (make-tree 28 nil nil) (make-tree 31 nil nil))))
;ta
(define tb (make-tree 3 (make-tree 1 nil nil) (make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))))
;tb
(define tc (make-tree 20 (make-tree 19 (make-tree 18 (make-tree 17 (make-tree 16 nil nil) nil) nil) nil) nil))
;tc
(define td (make-tree 20 (make-tree 19 (make-tree 18 (make-tree 17 (make-tree 16 nil nil) nil) nil) nil) nil))

; tree->list1 recurses left tree then right tree
; tree->list2 recurses right tree then left tree
; results are the same
;(tree->list1 tb)
;(tree->list2 tb)

; ex 2.63 part b: Do the two procedures have the same order of growth in the number of steps required to convert a
; tree with n elements to a list? no

; tree-list1 uses append to append left-branch to rest of tree; append grows O(n)
; append is called O(log n) times => n log n

; tree-list2 conses each element in the tree to result-list
; cons is used O(log n) times => log n

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (display "partial-tree ") (display elts) (display " ") (display n) (newline)
  (if (= n 0)
      (cons '() elts) ; no partial-tree is constructed
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
              (let ((left-tree (car left-result))
                    (non-left-elts (cdr left-result))
                    (right-size (- n (+ left-size 1))))
                    (display "this-entry is ") (display (car non-left-elts)) (newline)
                (let ((this-entry (car non-left-elts))
                      (right-result (partial-tree (cdr non-left-elts) right-size)))
                          (newline)
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (display "subtree is ") (display this-entry) (display left-tree) (display right-tree) (newline)
                    (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

 (define tree-list (tree->list2 tb))
 
 ;(list->tree tree-list); '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
 
; '(1 3 5 7 9 11)
 
;       5
;    /     \
;   1       9
;  / \    /   \
; ()  3   7    11
;    / \ / \   / \
;   () ()() () ()()
  
; 2.64 b: order of growth
; n = len(elements) => call partial-tree 2 ** log n times => O(n)
      
; 2.65 give O(n) implementations of union-set and intersection-set
; for sets implemented as (balanced) binary trees
 
(define (union-set-t set1 set2)
  (cond ((null? set1) (list->tree (tree->list2 set2)))
        ((null? set2) (list->tree (tree->list2 set1)))
        (else
         (let ((e1 (entry set1))
               (e2 (entry set2))
               (left1 (left-branch set1))
               (right1 (right-branch set1))
               (left2 (left-branch set2))
               (right2 (right-branch set2)))
           (cond ((= e1 e2) (make-tree e2 (union-set-t left1 left2) (union-set-t right1 right2)))
                 ((< e1 e2) 
                  (let ((s1 (union-set-t left1 right1))
                        (s2 (adjoin-set-t e1 set2)))
                    (union-set-t s1 s2)))
                 (else 
                  (let ((s1 (union-set-t left2 right2))
                        (s2 (adjoin-set-t e2 set1)))
                    (union-set-t s1 s2))))))))

(define (intersection-set-t set1 set2)
  (if (or (null? set1) (null? set2)) 
      '()
      (let ((e1 (entry set1))
            (e2 (entry set2))
            (left1 (left-branch set1))
            (right1 (right-branch set1))
            (left2 (left-branch set2))
            (right2 (right-branch set2)))
          (cond ((= e1 e2) (list->tree (tree->list2 (make-tree e1 (intersection-set-t left1 left2) (intersection-set-t right1 right2)))))
                ((< e1 e2) 
                 (let ((s1 (intersection-set-t right1 set2))
                       (s2 (intersection-set-t left1 left2)))
                (union-set-t s1 s2))); balanced tree
                (else
                 (let ((s1 (intersection-set-t set1 right2))
                       (s2 (intersection-set-t left1 left2)))
                (union-set-t s1 s2))))))); balanced tree
ta
tc
;(list->tree (tree->list2 tc))
(union-set-t ta tc)
; '(19 (16 (1 () (3 () ())) (17 () (18 () ()))) (27 (20 () (26 () ())) (30 (28 () ()) (31 () ()))))
(intersection-set-t tc td)
; '(18 (16 () (17 () ())) (19 () (20 () ())))

; Exercise 2.66. Implement the lookup procedure for the case where 
; the set of records is structured as a
; binary tree, ordered by the numerical values of the keys.

(define (make-record id lastname firstname)
  (list id lastname firstname))

(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        (else
         (let ((record (entry set-of-records))
               (left (left-branch set-of-records))
               (right (right-branch set-of-records)))
           (cond ((= given-key (key record)) record)
                 ((< given-key (key record)) (lookup given-key left))
                 (else (lookup given-key right)))))))

(define rec20 (make-record 20 'klein 'leslie))
(define rec21 (make-record 21 'kolchmeyer 'robert))
(define rec22 (make-record 22 'kolchmeyer 'david))

(define set-of-records (make-tree rec20 rec21 rec22))

;(lookup 20 set-of-records)
  


          