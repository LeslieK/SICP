;#lang racket
#lang planet neil/sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial_i n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (make-withdraw-2 balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;(define W1 (make-withdraw-2 100))

;(W1 50)

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
          
 ; Exercise 3.1
          
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))

;(define (make-cycle z)
;  (set-cdr! (last-pair x) x)
;  x)

;(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(define (count-pairs x)
;  (if (not (pair? x))
;      0
;      (+ (count-pairs (car x))
;         (count-pairs (cdr x))
;         1)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Ex. 3-17
(define (count-pairs x)
  (define (helper z aux)
    ;(display aux) (newline)
    (cond ((not (pair? z)) 0)
           ((memq z aux) 0)
           (else
              (if (null? aux)
                  (set! aux (list z))
                  (append! aux (list z)))
              (+ (helper (car z) aux)
                 (helper (cdr z) aux)
                 1))))
  (helper x '()))
        
(define x (list (list 1 2) 3 4))
(define z1 (cons x x))
;(count-pairs x)
;(count-pairs z1)

;; Ex. 3-18 and 3-19
;; define procedure to detect a cycle in a list
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (hasCycle? first)
  (define n1 first)
  (define n2 first)
  (define (helper n1 n2)
    (cond ((null? n2) #f)
          ((null? (cdr n2)) #f)
          (else 
           (let ((p1 (cdr n1))
                 (p2 (cddr n2)))
             (if (eq? p1 p2)
                 #t
                 (helper p1 p2))))))
  (helper n1 n2))

(define first (make-cycle (list 1 2 3 4 5)))
(hasCycle? x)
(hasCycle? z1)
(hasCycle? first)
  


