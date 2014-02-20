;#lang racket
#lang planet neil/sicp

;Exercise 3.23.  A deque (``double-ended queue'') is a 
;sequence in which items can be inserted and deleted at 
;either the front or the rear. Operations on deques are 
;the constructor make-deque, the predicate empty-deque?, 
;selectors front-deque and rear-deque, and mutators front-insert-deque!, 
;rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. 
;Show how to represent deques using pairs, and give implementations 
;of the operations.23 All operations should be accomplished in (1) steps.

(define (make-deque)
  (define (make-queue)
    (cons '() '()))
  (let ((count 0))
     (let ((qtop (make-queue))
           (qbot (make-queue)))
       (cons (cons qtop qbot) count))))

(define (front-insert-deque! deque item)
        (let ((node-top (car (make-node item)))
              (node-bot (cdr (make-node item))))
          (cond ((empty-deque? deque)
                 (init-deque deque node-top node-bot))
                (else
                 (set-cdr! node-top (front-ptr-top deque)) ; node-top is on list
                 (set-front-ptr-top! deque node-top)  ; front-ptr-top -> node-top
                 (set-cdr! (front-ptr-bot deque) node-bot) ; node-bot on list
                 (set-front-ptr-bot! deque node-bot) ; front-ptr-bot -> node-bot
                 (set-cdr! node-bot (front-ptr-top deque))
                 (incr deque)
                 ))))

(define (rear-insert-deque! deque item)
  (let ((node-top (car (make-node item)))
        (node-bot (cdr (make-node item))))          
        (cond ((empty-deque? deque)
               (init-deque deque node-top node-bot))
              (else
               (set-cdr! node-bot (rear-ptr-bot deque)) ; node-bot -> last-bot
               (set-rear-ptr-bot! deque node-bot) ; rear-ptr-bot -> node-bot
               (set-cdr! (rear-ptr-top deque) node-top) ; last-top -> node-top
               (set-rear-ptr-top! deque node-top) ; rear-ptr-top -> node-top (last)
               (set-cdr! (rear-ptr-top deque) node-bot) ; rear-ptr-top -> node-bot
               (incr deque)
               ))))
  
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue"))
        ((one-item? deque)
         (reset-deque! deque))    
        (else
         (set-front-ptr-top! deque (cdr (front-ptr-top deque))) ; front-ptr-top -> next
         (set-cdr! (front-ptr-bot deque) '()) ; prevent loitering ?
         (set-front-ptr-bot! deque (get-link-to-bot (front-ptr-top deque))) ; front-ptr-bot -> prev
         (set-cdr! (front-ptr-bot deque) (front-ptr-top deque)) ; bot -> top
         (decr deque)
         )))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue"))
        ((one-item? deque)
         (reset-deque! deque))
        (else
         (set-rear-ptr-bot! deque (cdr (rear-ptr-bot deque))) ; rear-ptr-bot -> next
         (set-cdr! (rear-ptr-top deque) '()) ; prevent loitering ?
         (set-rear-ptr-top! deque (get-link-to-top (rear-ptr-bot deque))) ; rear-ptr-top -> prev
         (set-cdr! (rear-ptr-top deque) (rear-ptr-bot deque)) ; top -> bot
         (decr deque)
         )))
         
(define (front-deque deque)
  (cond ((empty-deque? deque)'())
        (else
         (car (car (front-ptr-top deque)))))) ; item = ((1 2 3) . '()) front = 1

(define (rear-deque deque) 
  (cond ((empty-deque? deque) '())
        (else (car (car (rear-ptr-top deque))))))

(define (empty-deque? deque) 
  (= (size deque) 0))

; helper functions
(define (front-ptr-top deque) (car (car (car deque))))
(define (front-ptr-bot deque) (car (cdr (car deque))))
(define (rear-ptr-top deque) (cdr (car (car deque))))
(define (rear-ptr-bot deque) (cdr (cdr (car deque))))
(define (set-front-ptr-top! deque value) (set-car! (car (car deque)) value))
(define (set-rear-ptr-top! deque value) (set-cdr! (car (car deque)) value))
(define (set-front-ptr-bot! deque value) (set-car! (cdr (car deque)) value))
(define (set-rear-ptr-bot! deque value) (set-cdr! (cdr (car deque)) value))
(define (size deque) (cdr deque))

(define (get-link-to-bot ptr-top)
  (caddr (car ptr-top)))

(define (get-link-to-top ptr-bot)
  (cadr (car ptr-bot)))

(define (make-node item)
  (let ((new-node (list item '() '())))
    (let ((new-node-top (cons new-node '()))
          (new-node-bot (cons new-node '())))
      (set-car! (cdr new-node) new-node-top)
      (set-car! (cddr new-node) new-node-bot)
      (cons new-node-top new-node-bot))))
   
(define (init-deque deque node-top node-bot)
  (set-front-ptr-top! deque node-top); front-ptr-top -> node-top
  (set-rear-ptr-top! deque node-top) ; rear-ptr-top -> node-top
  (set-front-ptr-bot! deque node-bot) ; front-ptr-bot -> node-bot
  (set-rear-ptr-bot! deque node-bot) ; rear-ptr-bot -> node-bot
  (set-cdr! node-top node-bot) ; top -> bot
  (set-cdr! node-bot node-top) ; bot -> top
  (set-cdr! deque 1)
  )
  
(define (invar-true? deque)
  (cond ((null? (front-ptr-top deque)) (display "invar nil\n"))
        ((and (eq? (cdr (front-ptr-bot deque)) (front-ptr-top deque))
              (eq? (cdr (rear-ptr-top deque)) (rear-ptr-bot deque))) #t)
        (else #f)))

(define (one-item? deque)
  (= (size deque) 1))
  
(define (reset-deque! deque)
  (set-front-ptr-top! deque '())
  (set-rear-ptr-top! deque '())
  (set-front-ptr-bot! deque '())
  (set-rear-ptr-top! deque '())
  (set-cdr! deque 0))

(define (incr deque)
  (set-cdr! deque (+ (size deque) 1)))
  
(define (decr deque)
  (set-cdr! deque (- (size deque) 1)))


(display "####################end of tests #############################################\n")


; examples
(display "making deque dq ... \n")
(define dq (make-deque))
(display "front ") (front-deque dq) ; '()
(display "rear ") (rear-deque dq) ; '()
;
(display "insert front 1\n") (front-insert-deque! dq 1)
(display "insert front 2\n") (front-insert-deque! dq 2)
(display "insert front 3\n") (front-insert-deque! dq 3)
(display "insert front 4\n") (front-insert-deque! dq 4)
(display "insert rear 5\n") (rear-insert-deque! dq 5)
(display "insert front 6\n") (front-insert-deque! dq 6)
(display "insert rear 7\n") (rear-insert-deque! dq 7)
(display "insert front 8\n") (front-insert-deque! dq 8)
(display "insert rear 9\n") (rear-insert-deque! dq 9)
(display "insert front 10\n") (front-insert-deque! dq 10)
(display "insert rear 11\n") (rear-insert-deque! dq 11)
(display "insert front 12\n") (front-insert-deque! dq 12)
(display "insert rear 13\n") (rear-insert-deque! dq 13)
;
(display "front ") (front-deque dq)
(display "rear ") (rear-deque dq)
(display "size: ") (size dq)
;
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "size: ") (size dq)
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "size: ") (size dq)
;;
(display "front delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear ") (rear-deque dq)
;;
(display "insert rear 100\n") (rear-insert-deque! dq 100)
(display "insert rear 101\n") (rear-insert-deque! dq 101)
(display "insert rear 102\n") (rear-insert-deque! dq 102)
(display "insert rear 103\n") (rear-insert-deque! dq 103)
(display "front ") (front-deque dq)
(display "rear ") (rear-deque dq)

(display "rear-delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "rear-delete\n") (rear-delete-deque! dq)
(display "rear ") (rear-deque dq)
(display "rear-delete\n") (rear-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear ") (rear-deque dq)
;
(display "front-delete\n") (front-delete-deque! dq)
(display "front ") (front-deque dq)
(display "rear ") (rear-deque dq)
(display "size: ") (size dq)
;

