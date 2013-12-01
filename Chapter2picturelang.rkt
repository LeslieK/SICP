#lang racket/gui

; big idea: combinging painters vs combining painter operations
; a painter is a procedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the vector abstraction
; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect z)
  (car z))

(define (ycor-vect z)
  (cdr z))

(define (add-vect v w)
  (let ((vx (xcor-vect v))
        (vy (ycor-vect v))
        (wx (xcor-vect w))
        (wy (xcor-vect w)))
    (make-vect (+ vx wx) (+ vy wy))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
          
(define (sub-vect v w)
  (add-vect v (scale-vect -1 w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The frame abstraction
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge-1 frame)
  (cadr frame))

(define (edge-2 frame)
  (cadr (cdr frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge-1 frame))
               (scale-vect (ycor-vect v)
                           (edge-2 frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; alternative frame contracts
(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge-1-2 frame)
  (cadr frame))

(define (edge-2-2 frame)
 (cdr (cdr frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the segment abstraction
; Exercise 2-48
;(define p (make-vect x y))

(define (make-segment p1 p2)
    (list p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))

;; -------------------------------------------------------------------------
;; Setting up the drawing infrastructure specific to PLT Racket.
(define size-x 250)
(define size-y 250)

(define picture (make-object bitmap% size-x size-y))
(define bm-dc (make-object bitmap-dc% picture))
(send bm-dc clear)

(define frame-gui (new frame% 
                       [label "Picture Language"]
                       [width (+ size-x 10)]
                       [height (+ size-y 35)]))

(define canvas (new canvas%
                    [parent frame-gui]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-bitmap picture 0 0))]))

(define (draw-line v1 v2)
  (send bm-dc 
        draw-line 
        (* size-x (xcor-vect v1)) 
        (- size-y (* size-y (ycor-vect v1)))
        (* size-x (xcor-vect v2))
        (- size-y (* size-y (ycor-vect v2)))))

(send frame-gui show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; procedures for combining painters

(define (identity painter) painter)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;Exercise 2.44. Define the procedure up-split used by corner-split. 
;It is similar to right-split,
;except that it switches the roles of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs-2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
    
 ; Exercise 2.45    
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (split painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  procedures that transform painters

;The arguments to transform-painter are points (represented as vectors) that specify the corners of the
;new frame: When mapped into the frame, the first point specifies the new frame's origin and the other two
;specify the ends of its edge vectors. Thus, arguments within the unit square specify a frame contained
;within the original frame.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;Exercise 2.50. Define the transformation flip-horiz, which flips painters horizontally, and
;transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)   ; end of edge-1
                     (make-vect 1 0))) ; end of edge-2

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; various segments->painter procedures
; Exercise 2-49
(define (for-each proc items)
  (if (null? (cdr items))
      (proc (car items))
      (and (proc (car items)) (for-each proc (cdr items)))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; segment is wrt the unit square
(define painter-outline
  (let ((p1 (make-vect 0 0))
        (p2 (make-vect 1 0))
        (p3 (make-vect 0 1))
        (p4 (make-vect 1 1)))
    (segments->painter (list (make-segment p1 p2)
                             (make-segment p2 p4)
                             (make-segment p4 p3)
                             (make-segment p3 p1)))))

(define painter-X
  (let ((p1 (make-vect 0 0))
        (p2 (make-vect 1 0))
        (p3 (make-vect 0 1))
        (p4 (make-vect 1 1)))
    (segments->painter (list (make-segment p1 p4)
                             (make-segment p2 p3)))))

(define painter-diamond
  (let ((p1 (make-vect 0 .5))
        (p2 (make-vect .5 0))
        (p3 (make-vect .5 1))
        (p4 (make-vect 1 .5)))
    (segments->painter (list (make-segment p2 p4)
                             (make-segment p4 p3)
                             (make-segment p3 p1)
                             (make-segment p1 p2)))))

(define wave 
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         (make-segment (make-vect 0.50 0.70) (make-vect 0.35 0.75)) ;smile 1
         (make-segment (make-vect 0.50 0.70) (make-vect 0.65 0.75)) ;smile 2
         )))


; Exercise 2.51. Define the below operation for painters. Below takes two painters as arguments. The
;resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second
;painter in the top. Define below in two different ways -- first by writing a procedure that is analogous to
;the beside procedure given above, and again in terms of beside and suitable rotation operations (from
;exercise 2.50).   
  
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bot
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bot frame)
        (paint-top frame)))))

(define (below-2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

(define (below-3 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define right-split-2 (split beside below))
(define up-split-2 (split below beside))

; modify wave: lowest layer of language: manipulating vectors and segments

; modify corner-split: a pattern of combining a single painter using beside and below
; modify how painter is combined
(define (corner-split-2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split-2 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; modify square-limit: a pattern of transforming each of 4 painters before combining them into a square
; modify how squares are combined in combine4
(define (square-limit-3 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split ((flipped-pairs painter) n)))))


;;;;;;;;;;;;;;;;;;;;;;;;; draw something!

(define frame (make-frame (make-vect 0 0)
                          (make-vect 1 0)
                          (make-vect 0 1)))

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
(define wave4-1 (flipped-pairs wave))

(painter-diamond frame)