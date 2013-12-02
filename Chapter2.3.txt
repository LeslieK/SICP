#lang racket
;#lang planet neil/sicp

(define nil '())

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (head-of-list sep s)
  (define (helper sep alist s)
    (cond ((or (null? s) (not (list? s))) '()); return nil if no separator
          ((eq? (car s) sep) 
           (if (null? (cdr alist)); (x) only 1 item in list
               (car alist)
               alist))
          (else
           (let ((a (car s)))
             (cond ((eq? sep a) 
                    (if (not (pair? (cdr alist)))
                        (car alist)
                        alist))
                   (else (if (list? a)
                             (helper sep (append alist a (cdr s)))
                             (helper sep (append alist (list a)) (cdr s)))))))))
  (helper sep '() s))

(define (tail-of-list sep s)
  (let ((a (memq sep s)))
    (cond ((not (pair? (cddr a)))(cadr a))
          (else (cdr a)))))

; Ex 2.53
;(list 'a 'b 'c); '(a b c)

;(list (list 'george)); '((george))
;(cdr '((x1 x2) (y1 y2))); '((y1 y2))

;(cadr '((x1 x2) (y1 y2))); '(y1 y2)
;(pair? (car '(a short list))); #f
;(memq 'red '((red shoes) (blue socks))); #f
;(memq 'red '(red shoes blue socks)); '(red shoes blue socks)

; Ex 2.54
(define (equal? list1 list2)
  (cond ((null? list1) (null? list2))
        ((not (or (pair? list1) (pair? list2))) (eq? list1 list2)); both are symbols
        (else (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))))

;(equal? '(a b c) '(a b d))

; Ex 2.55
;(car ''abracadabra)
; first character in symbol is ', which is printed as the string quote
;'quote
;(car (list 'quote))

; Ex 2.56
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (simple-expr? exp)
  (not (pair? exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;(make-product 'a 3)

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
 
(define (augend-2 s) (caddr s))

; ex 2-57
(define (augend s);     (+ s1 s2 s3 s4) => (+ s2 s3 s4)
  (let ((s2 (cddr s))
        (s3 (cdddr s)))
    (cond ((null? s3) (car s2))
          (else (cons '+ s2)))))

(augend '(+ 1 2 3 4))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand-2 p) (caddr p))

; ex 2-57
(define (multiplicand p); (* m1 m2 m3 m4) => (* m2 m3 m4)
  (let ((m2 (cddr p))
        (m3 (cdddr p)))
    (cond ((null? m3) (car m2))
          (else (cons '* m2)))))

;(multiplicand '(* 1 2 'a 4 5))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        ((and (number? base) (number? exponent)) (fast-expt base exponent))
        (else (list '** base exponent))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
        (make-product (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                      (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x); deriv (* (* a b) c) x
(deriv '(* x y (+ x 3)) 'x); deriv (* a b c) x

; ex 2.56
(same-variable? (variable? (base (make-exponentiation 'u 4))) 'x)

;(deriv '(** u n) 'x)

; ex 2.57 (alt definition of augend, multiplicand above)
;(deriv '(* x y (+ x 3)) 'x)
;(deriv '(+ x x x) 'x)

; ex 2.58 part a: in-fix + and *

(define (make-sum-in a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-in m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum?-in x)
  (and (pair? x) (eq? (cadr x) '+)))

(display "sum?-in\n")
;(sum?-in '(a + (b + c)))

(define (product?-in x)
  (and (pair? x) (eq? (cadr x) '*)))

(display "product?-in\n")
;(product?-in (make-product-in 'm1 (make-product-in 'm2 'm3)))

(define (addend-in s)
  (car s))

(display "addend-in\n")
;(addend-in (make-sum-in 'a 'b))

(define (augend-in s); (a1 + (a2 + (a3 + ...)
  (caddr s))

(display "augend-in\n")
;(augend-in '(a + (b + c)))
;(augend-in (make-sum-in 'a (make-sum-in 'b (make-sum-in 'c 'd))))

(define (multiplier-in p)
  (car p))

(display "multiplier\n")
;(multiplier-in (make-product-in 'm1 'm2))

(define (multiplicand-in p); (m1 * (m2 * (m3 * m4)))
  (caddr p))

(display "multiplicand-in\n")
;(multiplicand-in (make-product-in 'm1 (make-product-in 'm2 'm3)))

; exercise 2.58 part b

(define (make-sum-in-2 a1 . a2)
  (define (helper a alist acc)
    (cond ((and (null? alist) (=number? a 0)) acc)
          ((and (null? alist) (=number? acc 0)) a)
          ((and (null? alist) (number? a) (number? acc)) (+ acc a))
          ((null? alist) 
                (if (list? acc) 
                    (append acc (list '+ a))
                    (append (list acc) (list '+ a))))
          (else
           (let ((p (car alist))
                 (q (cdr alist)))
             (cond ((=number? a 0) (helper p q acc))
                   ((=number? acc 0) (helper p q a))
                   ((and (number? a) (number? acc)) (helper p q (+ acc a)))
                   (else 
                    (if (list? acc) 
                        (helper p q (append acc (list '+ a)))
                        (helper p q (append (list acc) (list '+ a))))))))))
  (helper a1 a2 0))

(define (addend-in-2 s)
  (head-of-list '+ s))

(define (augend-in-2 s)
  (let ((a (memq '+ s)))
    (cond ((not (pair? (cddr a)))(cadr a))
          (else (cdr a)))))

(define (sum?-in-2 s)
  (memq '+ s))
                              
(define (make-product-in-2 a1 . a2)
  (define (helper a alist acc)
    (cond ((=number? a 0) 0)
          ((and (null? alist) (=number? a 1)) acc)
          ((and (null? alist) (=number? acc 1)) a)         
          ((and (null? alist) (number? a) (number? acc)) (* a acc))
          ((null? alist) 
                (if (list? acc) 
                    (append acc (list '* a))
                    (append (list acc) (list '* a))))
          (else
           (let ((p (car alist))
                 (q (cdr alist)))
             (cond ((=number? a 1) (helper p q acc))
                   ((=number? acc 1) (helper p q a))
                   ((and (number? a) (number? acc)) (helper p q (* acc a)))
                   (else 
                    (if (list? acc) 
                        (helper p q (append acc (list '* a)))
                        (helper p q (append (list acc) (list '* a))))))))))
  (helper a1 a2 1))

(define (multiplier-in-2 p)
  (head-of-list '* p))

(define (multiplicand-in-2 p)
  (tail-of-list '* p))

(define (product?-in-2 p)
  (memq '* p))

(define (deriv-2 exp var)
  (display "exp is ") (display exp) (newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-in-2 exp)
         (display "is a sum") (newline)
         (make-sum-in-2 (deriv-2 (addend-in-2 exp) var)
                   (deriv-2 (augend-in-2 exp) var)))
        ((product?-in-2 exp)
         (display "is a product") (newline)
         (make-sum-in-2
          (make-product-in-2 (multiplier-in-2 exp)
                        (deriv-2 (multiplicand-in-2 exp) var))
          (make-product (deriv-2 (multiplier-in-2 exp) var)
                        (multiplicand-in-2 exp))))
        ((exponentiation? exp)
        (make-product-in-2 (make-product-in-2 (exponent exp) (make-exponentiation (base exp) (make-sum-in-2 (exponent exp) -1)))
                      (deriv-2 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV-2" exp))))


;(define s (make-sum-in-2 'x 5))
;(augend-in-2 s)
;(make-sum-in-2 0 2 'x 0 5 0 'y 0 0 'q)
;(addend-in-2 (make-sum-in-2 1 2 3 4 5 'y 'z 1 2 3))
;(augend-in-2 (make-sum-in-2 1 2 3 4 5 'y 'z 1 2 3))
;(sum?-in-2 (make-sum-in-2 1 2 3 4 5 'y 'z 1 2 3))
;(make-sum-in-2 'x 'y 'z 'r 's 't) 
;(define p (make-product-in-2 'a 2))
;p
;(product?-in-2 p)
;(multiplier-in-2 p)
;(multiplicand-in-2 p)
;(product?-in-2 (make-product-in-2 5 4 3 2 'x 'y 1 'z 's 1 't))
;(multiplier-in-2 (make-product-in-2 5 4 3 2 'x 'y 1 'z 's 1 't))
;(multiplicand-in-2 (make-product-in-2 5 4 3 2 'x 'y 1 'z 's 1 't))
(define e (make-sum-in-2 (make-product-in-2 2 3 4 'x) (make-product-in-2 5 'x)))
e
;(memq '+ e)
;(augend-in-2 e)
;(memq '+ (make-sum-in-2 'a 'b 'c))
;(addend-in-2 '(3 + b + 1))
;(augend-in-2 '(3 + b + 1))
;(addend-in-2 '(6 * 1 * x + 4 * 3 * z))
;(augend-in-2 '(6 * 1 * x + 4 * 3 * z))
;(addend-in-2 '(x + 3))
;(augend-in-2 '(x + 3))
;(addend-in-2 '(6 * y + 5 * x + 3))
;(augend-in-2 '(6 * y + 5 * x + 3))
;(multiplier-in-2 120)
;(make-product-in-2 6 5 4)
;(multiplier-in-2 '(5 + 4 * 3 + x + y))
;(multiplicand-in-2 '(5 + 4 * 3 + x + y))
;(product?-in-2 '(24 * x))
(deriv-2 e 'x)
;(memq '+ (make-product-in-2 24 'x))
;(sum?-in-2 (make-product-in-2 24 'x))
(deriv-2 '(x + 3 * (x + y + 2)) 'x)