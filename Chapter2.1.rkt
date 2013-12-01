;#lang racket
#lang planet neil/sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;(define (make-rat n d) (cons n d))
;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))

; ex2.1
; Define a better version of make-rat that handles both positive and negative arguments.


(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (or (and (< n 0) (> d 0)) (and (> n 0) (< d 0))) 
        (cons (/ (- (abs n)) g) (/ (abs d) g))
        (cons (/ (abs n) g) (/ (abs d) g)))))
      
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat 1 3))
;(print-rat one-third) 

; ex2.2
(define(make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))

;(define (midpoint-segment line)
;  (define mid-x (average (x-point (start-segment line))
;                         (x-point (end-segment line))))
;  (define mid-y (average (y-point (start-segment line))
;                         (y-point (end-segment line))))
;  (make-point mid-x mid-y))

(define (midpoint-segment line)
  (let ( (mid-x (average (x-point (start-segment line)) (x-point (end-segment line))))
         (mid-y (average (y-point (start-segment line)) (y-point (end-segment line)))))
    (make-point mid-x mid-y)))
  
;(print-point (midpoint-segment (make-segment (make-point -5 0) (make-point 4 -5))))
   
;(print-point (make-point 1 2))

; ex2.3
; implement a representation for rectangles in a plane

; a rect representation (lower-left-point upper-right-point)
(define (make-rect lower-left-point upper-right-point)
  (cons lower-left-point upper-right-point))
  
(define (height rect)
  (let ((y1 (y-point (car rect)))
        (y2 (y-point (cdr rect))))
    (- y2 y1)))
  
(define (width1 rect)
  (let ((x1 (x-point (car rect)))
        (x2 (x-point (cdr rect))))
    (- x2 x1)))

(define (area rect)
  (let ((width (width1 rect))
        (height (height rect)))
    (* width height)))

(define (perimeter rect)
   (let ((width (width1 rect))
         (height (height rect)))
    (+ (* 2 width) (* 2 height))))

;(area (make-rect (make-point 0 0) (make-point 5 5)))
;(perimeter (make-rect (make-point 0 0) (make-point 5 5)))
 
;; an alternative rect representation (origin (width height))
(define (make-rect-2 lower-left upper-right)
  (let ((x1 (x-point lower-left))
        (y1 (y-point lower-left))
        (x2 (x-point upper-right))
        (y2 (y-point upper-right)))
    (cons lower-left (cons (- x2 x1) (- y2 y1))))) ; (origin (width height))

(define (width-2 rect)
  (car (cdr rect)))

(define (height-2 rect)
  (cdr (cdr rect)))
  
;(area (make-rect-2 (make-point 0 0) (make-point 5 5)))
;(perimeter (make-rect (make-point 0 0) (make-point 5 5)))

;; ex2-4
(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

;(cons2 4 5)
;(car2 (cons2 4 5))
;(cdr2 (cons2 4 5))

;; ex2-5 
(define (cons3 x y)
  (* (fast-expt 2 x) (fast-expt 3 y)))

(define (car3 z)
  (define (helper z count)
    (if (even? z)
        (helper (/ z 2) (+ count 1))
        count))
  (helper z 0))

(define (cdr3 z)
  (define (helper z count)
    (if (= (remainder z 3) 0)
        (helper (/ z 3) (+ count 1))
        count))
  (helper z 0))

; ex2-6
(define zero (lambda (f) (lambda (x) x)))  ; f => Identity

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))) ; f => f(x) (add-1 zero)  f => f(f(x)) (add-1 (add-1 n))

(define one (lambda (f) (lambda (x) (f x)))) ; f => f(x)
(define two (lambda (f) (lambda (x) (f (f x))))) ; f => f(f(x))

(define (myadd m n) 
  (lambda (f) (lambda (x) ((m f)((n f) x)))))
  
;; (myadd zero zero) => ((zero f) ((zero f) x)) => x

;; (myadd one zero) => ((one f) ((zero f) x)) => ((one f) x) => (f x)
;; (myadd zero one) => ((zero f) ((one f) x)) => ((lambda x x) (f x)) => (f x)
;; (myadd one one) => ((one f) ((one f) x)) => ((one f) ((lambda x (f x)) x)) => ((one f) (f x)) => ((lambda x (f x)) (f x) => (f f(x)) => f(f(x))

; ex2-7
; (lower-bound, upper-bound)
(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

; ex2-8
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; ex2-9
;; addition: x=(c, d) y=(a, b) x+y=(c + a, d + b) (d + b) - (c + a) = (d - c) + (b - a) = 2(wx + wy) => width of sum = wx + wy
;; multiplication: x=(c, d) y=(a, b) x=(1, 3) y=(2, 4) x*y = (2, 12) width = 5 wx = 1, wy = 1
;; multiplication: x=(1, 2) y = (2, 6) x*y = (2, 12) width = 5 (wx = .5, wy = 2)

; ex2-10
; check for divide by zero
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (display "Error: divide by 0\n")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;(div-interval (make-interval 0 1) (make-interval -1 1))
;(div-interval (make-interval 0 1) (make-interval -1 0))
;(div-interval (make-interval 0 1) (make-interval 0 1))
;(div-interval (make-interval 0 1) (make-interval 1 10))


; ex2-11
(define (pos? n) (>= n 0))
(define (neg? n) (< n 0))

(define (pos-int? x)
  (if (and (pos? (lower-bound x)) (pos? (upper-bound x)))
      true
      false))
(define (neg-int? x)
  (if (and (neg? (lower-bound x)) (neg? (upper-bound x)))
      true
      false))

(define (neg-pos? x)
  (if (and (not (pos-int? x)) (not (neg-int? x)))
      true
      false))

(define (mult-int x y)
  (let ((p1 (lower-bound x))
        (p2 (upper-bound x))
        (p3 (lower-bound y))
        (p4 (upper-bound y)))
    (cond ((and (pos-int? x) (pos-int? y)) (make-interval (* p1 p3) (* p2 p4)))
          ((and (pos-int? x) (neg-int? y)) (make-interval (* p2 p3) (* p1 p4)))
          ((and (pos-int? x) (neg-pos? y)) (make-interval (* p2 p3) (* p2 p4)))
          ((and (neg-int? x) (pos-int? y)) (make-interval (* p1 p4) (* p2 p3)))
          ((and (neg-int? x) (neg-int? y)) (make-interval (* p1 p3) (* p2 p4)))
          ((and (neg-int? x) (neg-pos? y)) (make-interval (* p1 p4) (* p1 p3)))
          ((and (neg-pos? x) (pos-int? y)) (make-interval (* p1 p4) (* p2 p4)))
          ((and (neg-pos? x) (neg-int? y)) (make-interval (* p2 p3) (* p1 p3)))
          ((and (neg-pos? x) (neg-pos? y)) 
           (make-interval (min (* p2 p3) (* p1 p4)) (max (* p2 p4) (* p2 p3)))))))
        
;(mult-int (make-interval 5 10) (make-interval 6 20))
;(mult-int (make-interval 6 20) (make-interval 5 10))
;(mult-int (make-interval 5 10) (make-interval -20 -6))
;(mult-int (make-interval 5 10) (make-interval -6 20))

;(mult-int (make-interval -10 -5) (make-interval 6 20))
;(mult-int (make-interval -10 -5) (make-interval -20 -6))
;(mult-int (make-interval -10 -5) (make-interval -6 20))

;(mult-int (make-interval -5 10) (make-interval 6 20))
;(mult-int (make-interval -5 10) (make-interval -20 -6))
;(mult-int (make-interval -5 10) (make-interval -6 20))


     
;; ex2-12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((width (* c (/ p 100.)))) 
        (make-center-width c width)))

;(make-center-percent 6.8 10)

(define (percent i)
    (* (/ (width i) (center i)) 100))

;(percent (make-center-percent 6.8 10))

; ex 2-13
;p1 = tolerance of R1
;p2 = tolerance of R2
;tolerance of R1*R2 = p1 + p2

;(percent (mult-int (make-center-percent 6.8 3) (make-center-percent 6.8 3)))

; ex 2-14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
 ; large tolerance
(define R1 (make-center-percent 8 1.5))
(define R2 (make-center-percent 6 1))
(/ (percent (par1 R1 R2)) (percent (par2 R1 R2))) ;3.116

; small tolerance
(define rx (make-center-percent 50000 .001))
(define ry (make-center-percent 80000 .002))
(/ (percent (par1 rx ry)) (percent (par2 rx ry))) ; 3.333

(define A (make-interval 3 6)) ; w = 1.5 (p = 33 1/3)
;(width A)
;(percent A)
(define B (make-interval 4 5)) ; w = .5  (p = 11 1/9)
;(width B)
;(percent B)
;(div-interval A A)  ; A / A (.5, 2)  w = .72 p = 60
;(width (div-interval A A))
;(percent (div-interval A A))
;(div-interval B B)  ; B / B (.8, 1.25) w = .224 p = 21
;(width (div-interval B B))
;(percent (div-interval B B))
;(div-interval A B)  ; (.6 1.5)  ; (.6 1.5)
;(div-interval (make-center-percent 1 0) (div-interval B A))  ; (.6 1.5)

(define C (make-center-percent 2 .01))
(define D (make-center-percent 3 .01))
;(par1 C D) ; (1.1996 1.2003)
;(par2 C D) ; (1.1998 1.2001)

;(div-interval C C) ; match on lower bounds /  slightly diff on upper bounds
;(div-interval D D) ;

(percent (mul-interval C C)) ; .01999
(percent (div-interval (mul-interval C C) C)) ; .029
(percent (sub-interval (add-interval C C) C)) ; .029
(percent (div-interval (mul-interval C C) (mul-interval C C))) ; .039

(percent (div-interval (mul-interval D D) D)) ; .029  

; ex2-15
; par2 is better because the uncertainty increases when intervals are used in 
; arithmetic expressions and par2 uses each interval once.
; wikipedia: If an interval occurs several times in a calculation using parameters, and 
; each occurrence is taken independently then this can lead to an unwanted expansion of the resulting intervals.
;In general, it can be shown that the exact range of values can be achieved, if each variable appears only once. 
;However, not every function can be rewritten this way.
; 2-16

        