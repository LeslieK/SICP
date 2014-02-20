;#lang racket
#lang planet neil/sicp
#(provide (all-defined-out))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called on empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (else
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

;; Ex. 3.21
(define (print-queue queue)
  (for-each (lambda (x) (display x) (display " " )) (front-ptr queue))
  (newline))

(define myq (make-queue))
(insert-queue! myq 3)
(insert-queue! myq 2)
(insert-queue! myq 99)
(insert-queue! myq 98)
(print-queue myq)

;(define q1 (make-queue))
;(insert-queue! q1 'a)
;;((a) a)
;(insert-queue! q1 'b)
;;((a b) b)
;(delete-queue! q1)
;;((b) b)
;(delete-queue! q1)
;;(() b)
; queue is a pair. 
; front-ptr is a reference to the entire list (a b)
; to delete, front-ptr is set to the cdr of the list
; the last delete, sets front-ptr to ptr to nil (that explains (() b)
; rear-ptr references the last item, which is the pair (cons elem '()), 
; represented as item
; when the queue is empty, rear-ptr still points to the last item

;; Ex. 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    
    (define (empty-queue?) (null? front-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called on empty queue")
          (car front-ptr)))
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)  ; this does the first append
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair) ; this does the append
               (set! rear-ptr new-pair)
               front-ptr))))
    
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)) ; this does the delete
             front-ptr)))
    
    (define (print-queue)
      (for-each (lambda (x) (display x) (display " " )) front-ptr)
      (newline))
    
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'print-queue) print-queue)))
    dispatch))

(define myq2 (make-queue2))
((myq2 'insert-queue!) 42)
((myq2 'insert-queue!) 43)
((myq2 'insert-queue!) 44)
((myq2 'print-queue))
((myq2 'delete-queue!))
((myq2 'print-queue))
((myq2 'empty-queue?))

