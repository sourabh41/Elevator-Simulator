#lang racket
(require racket/gui)
(require racket/draw/arrow)
(require racket/vector)
(require "declarations.rkt")
(require "classes.rkt")

(provide frame)

;;frame
(define frame (new frame%
                   [label "Elevator Simulator"]
                   [width 1350]
                   [height 700]))

;;extra
(define extra 2)


;;size of box : a
(define (size n m)
  (let* ((length (+ 10 (* 3 n)))
         (breadth  (+ 7 (+ (* 3 m) (* m (quotient n 5))))))
    (cond [(< (quotient 1350 length)  (quotient 750 breadth)) (quotient 1350 length)]
          [else (quotient 750 breadth)])))
(define (a n m)   (if (> (size no-of-floors no-of-lifts) 25) 25
                                   (size no-of-floors no-of-lifts))) 


;;important functions for lifts
(define lifts (make-vector no-of-lifts (* 10 (a no-of-floors no-of-lifts))))

(define (current-posn i)
  (vector-ref lifts (- i 1)))

(define (change-posn i floor)
  (vector-set! lifts (- i 1) (* (a no-of-floors no-of-lifts) (+ 7 (* 3 floor)))))

(define (set-positions)
  (define (help i)
    (if (= i no-of-lifts) (change-posn i (send (list-ref list-of-lifts (- i 1)) get-position))
        (begin (change-posn i (send (list-ref list-of-lifts (- i 1)) get-position))
               (help (+ i 1)))))
  (help 1))
(set-positions)

;;lift arrow
(define lift-movement (make-vector  no-of-lifts 'down))
(define (change-direction mthlift direction)
  (vector-set! lift-movement (- mthlift 1) direction))

(define (set-directions)
  (define (help i)
    (if (= i no-of-lifts) (change-direction i (send (list-ref list-of-lifts (- i 1)) get-direction))
        (begin (change-direction i (send (list-ref list-of-lifts (- i 1)) get-direction))
               (help (+ i 1)))))
  (help 1))

(set-directions)

(define (arrow x y direction)
  (let* (( a (a no-of-floors no-of-lifts)))
  (cond [(eq? direction 'up) (begin (send dc set-pen "blue" 3 'solid)
                                          (send dc draw-line (+ x 4) (+ y (/ a 2)) (+ x (- a 4)) (+ y (/ a 2)))
                                          (send dc draw-line (+ x (/ a 2)) (+ y 4) (+ x (- a 4)) (+ y (/ a 2)))
                                          (send dc draw-line (+ x (/ a 2)) (+ y (- a 4)) (+ x (- a 4)) (+ y (/ a 2)))
                                          (send dc set-pen "black" 3 'solid))]
        [(eq? direction 'down) (begin (send dc set-pen "blue" 3 'solid)
                                          (send dc draw-line (+ x 4) (+ y (/ a 2)) (+ x (- a 4)) (+ y (/ a 2)))
                                          (send dc draw-line (+ x (/ a 2)) (+ y 4) (+ x 4) (+ y (/ a 2)))
                                          (send dc draw-line (+ x (/ a 2)) (+ y (- a 4))  (+ x 4) (+ y (/ a 2)))
                                          (send dc set-pen "black" 3 'solid))]
        [else (begin (send dc set-brush "blue" 'solid)
                     (send dc draw-ellipse (+ x (/ a 2)) (+ y (/ a 2)) 5 5)
                     (send dc set-brush no-brush))])))

;;outer button colours
(define up-color (make-vector no-of-floors "GREEN"))

(define down-color (make-vector no-of-floors "GREEN"))

(define (change-color i dir color)
  (if (eq? dir "up") (vector-set! up-color (- i 1) color)
      (vector-set! down-color (- i 1) color)))

(define (set-up-colors)
  (define (help i)
    (if (= i no-of-floors) (change-color i "up" (if (send (list-ref list-of-ups (- i 1)) get-pressed?) "RED" "GREEN"))
        (begin (change-color i "up" (if (send (list-ref list-of-ups (- i 1)) get-pressed?) "RED" "GREEN"))
               (help (+ i 1)))))
  (help 1))

(define (set-down-colors)
  (define (help i)
    (if (= i no-of-floors) (change-color i "down" (if (send (list-ref list-of-downs (- i 1)) get-pressed?) "RED" "GREEN"))
        (begin (change-color i "down" (if (send (list-ref list-of-downs (- i 1)) get-pressed?) "RED" "GREEN"))
               (help (+ i 1)))))
  (help 1))

(set-up-colors)
(set-down-colors)


;;grid
(define (grid n m)
  (let* ((a (a n no-of-lifts))
         (b (+ (*  n 3) 13)))
    (define (help i)
      (if (= i 56) (begin (send dc draw-line 0 750 1400 750)
                          (send dc draw-line 1400 0 1400 750)) 
          (if (< i 30)
              (begin (send dc draw-line (* i 25) 0 (* i 25) 700)
                     (send dc draw-line 0 (* i 25) 1350 (* i 25))
                     (help (+ i 1)))
              (begin (send dc draw-line (* i 25) 0 (* i 25) 700)
                     (help (+ i 1))))))
    (define (new-help i)
      (if (= i b) (begin (send dc draw-line 0 750 1400 750)
                         (send dc draw-line 1400 0 1400 750)) 
          (if (< i (quotient 700 a))
              (begin (send dc draw-line (* i a) 0 (* i a) 700)
                     (send dc draw-line 0 (* i a) 1350 (* i a))
                     (new-help (+ i 1)))
              (begin (send dc draw-line (* i a) 0 (* i a) 700)
                     (new-help (+ i 1))))))
    (if  (< n 14)
         (help 0)
         (new-help 0))))


;;brushes
(define blue-brush (make-object brush% "BLUE" 'solid))
(define green-brush (make-object brush% "GREEN" 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))


;;drawing of triangles (outer buttons)
(define (draw-up-not-c x y)
  (send dc draw-polygon (list (cons (+ x 4) (+ y (- (a no-of-floors no-of-lifts) 4)))
                              (cons (+ x (/ (- (a no-of-floors no-of-lifts) 2) 2)) (+ y 6))
                              (cons (- (+ x (- (a no-of-floors no-of-lifts) 2)) 4) (+ y (- (a no-of-floors no-of-lifts) 4)))))
  
  (send dc set-brush no-brush))

(define (draw-down-not-c x y)
  (send dc draw-polygon (list (cons (+ x 4) (+ y 4))
                              (cons (+ x (/ (- (a no-of-floors no-of-lifts) 2) 2)) (+ y (- (a no-of-floors no-of-lifts) 6)))
                              (cons (- (+ x (- (a no-of-floors no-of-lifts) 2)) 4) (+ y 4))))
  (send dc set-brush no-brush))


;;grid containing outer buttons
(define (out-grid n m)
  (let* ((a (a n no-of-lifts))
         (k (k n)))
    (send dc set-pen "black" 2 'solid)
    (send dc draw-line (* 10 a) (* 3 a) (* a (+ 8 (* 3 n))) (* 3 a))
    (send dc draw-line (* 10 a) (* 4 a) (* a (+ 8 (* 3 n))) (* 4 a))
    (send dc draw-line (* 10 a) (* 3 a) (* 10 a) (* 6 a))
    (send dc draw-line (* a (+ 8 (* 3 n))) (* 3 a) (* a (+ 8 (* 3 n))) (* 6 a))
    (send dc draw-line (* 10 a) (* 6 a) (* a (+ 8 (* 3 n))) (* 6 a))
    (define (help y i)
      (if (= i (- n 1)) (send dc draw-text (number->string n) (+ (* a (+ 10 (* 3 i))) 2) (+ y (* 3 a)))
          (begin (send dc draw-text (number->string (+ i 1)) (+ (* 10 a) (+ (* 3(* i a)) 2)) (+ y (* 3 a)))
                 (help y (+ i 1)))))
    
    (define (lift p)
      (send dc set-pen "black" 3 'solid)
      (if (= p 0)
          (help 0 0)
          (begin
            (arrow (vector-ref lifts (- p 1)) (* (+ 9 (* (+ k 2) (- p 1))) a) (vector-ref lift-movement (- p 1)))
            (send dc draw-rectangle (vector-ref lifts (- p 1)) (* (+ 10 (* (+ k 2) (- p 1))) a) a a)  
            (help  (* (+ 7  (* (+ k 2) (- p 1))) a) 0)
            (lift (- p 1))))
      (send dc set-pen "black" 2 'solid))
    (lift m)
    (define (u-as i)
      (if (= i (- no-of-floors 1)) (begin (send dc set-brush (vector-ref up-color (- i 1)) 'solid)
                                          (draw-up-not-c (+ (* (+ 7 (* 3 i)) a) 2) (* 4 a)))
          (begin (send dc set-brush (vector-ref up-color (- i 1)) 'solid)
                 (draw-up-not-c (+ (* (+ 7 (* 3 i)) a) 2) (* 4 a))
                 (u-as (+ i 1)))))
    (u-as 1)
    (define (u-ds i)
      
      (if (= i  no-of-floors) (begin (send dc set-brush (vector-ref down-color (- i 1)) 'solid)
                                     (draw-down-not-c (+ (* (+ 7 (* 3 i)) a) 2)  (* 5 a)))
          (begin (send dc set-brush (vector-ref down-color (- i 1)) 'solid)
                 (draw-down-not-c (+ (* (+ 7 (* 3 i)) a) 2)  (* 5 a))
                 (u-ds (+ i 1))))
      )
    (u-ds 2)
    ))


;;checks height of lift
(define (k n)
  (if (= 0 (remainder n 5)) (quotient n 5)
      (+ 1 (quotient n 5))))


;function to check piont in a rectangle (required for click events)
(define (in-square particle-x particle-y box-x box-y)
  (let* ((a (a no-of-floors no-of-lifts)))
    (cond[(and (and (> particle-x box-x) (< particle-x (+ box-x 25))) (and (> particle-y box-y) (< particle-y (+ box-y 25)))) #t]
         [else #f])))


;click event
(define (click-event event)
  (cond[(equal? 'left-down (send event get-event-type)) #t]
       [else #f]))


;;making lifts
(define (lift-grid n y)
  (let* ((k (k n))
         ( a (a n no-of-lifts)))
    (send dc set-pen "black" 2 'solid)
    (send dc draw-line (* 3 a) y (* 8 a) y)
    (send dc draw-line (* 3 a) y (* 3 a) (+ y (* k a)))
    (send dc draw-line (* 3 a) (+ y (* k a)) (* 8 a) (+ y (* k a)))
    (send dc draw-line (* 8 a) y (* 8 a) (+ y (* k a)))
    (define (help i z x y k)
      (if (or (= i z) (= k 5)) (send dc draw-text (number->string i) (+ (* a (- k 1)) x) y)
          (begin (send dc draw-text (number->string i) (+ x (* (- k 1) a)) y)
                 (help (+ i 1) z x y (+ k 1)))))
    (define (call-helper p)
      (if (= p k) (help (+ 1 (* (- k 1) 5)) n (+ (* 3 a) 2) (+ y (* a (- k 1))) 1)
          (begin (help (+ 1 (* (- p 1) 5)) (+ 1 (* p 5))  (+ (* 3 a) 2) (+ y (* a (- p 1))) 1)
                 (call-helper (+ p 1)))))
    (call-helper 1)))


(define (all-liftgrid n m)
  (let* ((k (k n))
         (a (a n no-of-lifts)))
    
    (define (helper i)
      (if (= i m) (begin (lift-grid n (* (+ 9  (* (+ k 2) (- m 1))) a))
                         (send dc draw-text (string-append "Distance : " (number->string (* 1.0 (send (list-ref list-of-lifts (- i 1)) get-distance)))) (+ (* 3 a) 2) (- (* (+ 9  (* (+ k 2) (- i 1))) a) a)))
          (begin (lift-grid n (* (+ 9  (* (+ k 2) (- i 1))) a))
                 (send dc draw-text (string-append "Distance : " (number->string (* 1.0 (send (list-ref list-of-lifts (- i 1)) get-distance)))) (+ (* 3 a) 2) (- (* (+ 9  (* (+ k 2) (- i 1))) a) a))                 (helper (+ i 1)))))
    (helper 1)))


;;used to make red rectangles
(define (rectangle x y)
  (send dc set-pen "red" 2 'solid)
  (send dc draw-rectangle x y (a no-of-floors no-of-lifts) (a no-of-floors no-of-lifts))
  (send dc set-pen "grey" 0.25 'hilite))


;;inner button coloring  (to be corrected)
(define (set-inner-colors)
  (define (rectangle-maker m n)
    (let ([a (a no-of-floors no-of-lifts)])
      (if (= 0 (remainder no-of-floors 5))
          (if (= 0 (remainder n 5))
              (if (send (list-ref list-of-lifts (- m 1)) get-inner-button n) (rectangle (* 7 a) (* (+ 8 (+ (* (+ 2 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                  (set! extra 4))
              (if (send (list-ref list-of-lifts (- m 1)) get-inner-button n) (rectangle (* (+ 2 (remainder n 5)) a) (* (+ 9 (+ (* (+ 2 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                  (set! extra 4)))
          (if (= 0 (remainder n 5))
              (if (send (list-ref list-of-lifts (- m 1)) get-inner-button n) (rectangle (* 7 a) (* (+ 8 (+ (* (+ 3 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                  (set! extra 4))
              (if (send (list-ref list-of-lifts (- m 1)) get-inner-button n) (rectangle (* (+ 2 (remainder n 5)) a) (* (+ 9 (+ (* (+ 3 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                  (set! extra 4))))))
  (define (loop i j)
    (if (= i no-of-lifts)
        (if (= j no-of-floors) (rectangle-maker i j)
            (begin (rectangle-maker i j) (loop i (+ j 1))))
        (if (= j no-of-floors) (begin (rectangle-maker i j) (loop (+ i 1) 1))
            (begin (rectangle-maker i j) (loop i (+ j 1))))))
  (loop 1 1))

(set-inner-colors)


;;;function of mth lift and nth floor (inner buttons)
(define (click x y m n)
  (let* ((a (a no-of-floors no-of-lifts)))
    (if (= 0 (remainder no-of-floors 5))
        (if (= 0 (remainder n 5)) (cond [(in-square x y (* 7 a) (* (+ 8 (+ (* (+ 2 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                                         (begin (send (list-ref list-of-lifts (- m 1)) press n)
                                                (send dc draw-text (string-append (number->string m) (string-append "   " (number->string n)))  0 0))]
                                        [else (set! extra 4)])
            (cond [(in-square x y (* (+ 2 (remainder n 5)) a) (* (+ 9 (+ (* (+ 2 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                   (begin (send (list-ref list-of-lifts (- m 1)) press n)
                          (send dc draw-text (string-append (number->string m) (string-append "   " (number->string n)))  0 0))]
                  [else (set! extra 4)]))
        (if (= 0 (remainder n 5)) (cond [(in-square x y (* 7 a) (* (+ 8 (+ (* (+ 3 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                                         (begin (send (list-ref list-of-lifts (- m 1)) press n)
                                                (send dc draw-text (string-append (number->string m) (string-append "   " (number->string n)))  0 0))]
                                        [else (set! extra 4)])
            (cond [(in-square x y (* (+ 2 (remainder n 5)) a) (* (+ 9 (+ (* (+ 3 (quotient no-of-floors 5)) (- m 1)) (quotient n 5))) a))
                   (begin (send (list-ref list-of-lifts (- m 1)) press n)
                          (send dc draw-text (string-append (number->string m) (string-append "   " (number->string n)))  0 0))]
                  [else (set! extra 4)])))))


;stops of a lift
(define (liftpath i l)
  (let* ((a (a no-of-floors no-of-lifts)))
    
    (if (null? l)(set! extra 4)
        (if (= (remainder no-of-floors 5) 0)
            (begin (rectangle (* (+ 10 (* (- (car l) 1) 3)) a)  (* (+ 10 (* (+ 2 (quotient no-of-floors 5)) (- i 1))) a))
                   (liftpath i (cdr l)))
            (begin (rectangle (* (+ 10 (* (- (car l) 1) 3)) a) (* (+ 10 (* (+ 3 (quotient no-of-floors 5)) (- i 1))) a) )
                   (liftpath i (cdr l)))))))

(define (showpath)
  (define (help i)
    (if (= i no-of-lifts) (liftpath i (send (list-ref list-of-lifts (- i 1)) get-destination-list))
        (begin (liftpath i (send (list-ref list-of-lifts (- i 1)) get-destination-list))
               (help (+ i 1)))))
  (help 1))



;;button functions
(define (canvas-key frame)
  (class canvas%
    (define/override (on-event mouse-event)
      (let* ((x (send mouse-event get-x))
             (y (send mouse-event get-y)))
        ;;;;;outside buttons 
        (define (outside-button j)
          (let* ((a (a no-of-floors no-of-lifts)))
            (if (= j no-of-floors) (cond [(in-square x y (* (+ 7 (* 3 j)) a) (* 5 a))
                                          (begin (send dc draw-text (string-append (number->string j) " down")  0 0)
                                                 (send (list-ref list-of-downs (- j 1)) press))]
                                         [else (set! extra 4)])
                (if (= j 1) (begin (cond [(in-square x y (* (+ 7 (* 3 j)) a) (* 4 a))
                                          (begin (send dc draw-text (string-append (number->string j) " up")  0 0)
                                                 (send (list-ref list-of-ups (- j 1)) press))]
                                         [else (set! extra 4)]) (outside-button (+ j 1)))
                    
                    (begin (cond [(in-square x y (* (+ 7 (* 3 j)) a) (* 4 a))
                                  (begin (send dc draw-text (string-append (number->string j) " up")  0 0)
                                         (send (list-ref list-of-ups (- j 1)) press))]
                                 [(in-square x y (* (+ 7 (* 3 j)) a) (* 5 a))
                                  (begin (send dc draw-text (string-append (number->string j) " down")  0 0)
                                         (send (list-ref list-of-downs (- j 1)) press))]
                                 [else (set! extra 4)]) (outside-button (+ j 1)))))))
        (define (mhelper j)
          (define (helper i m)
            (if (= i no-of-floors) (click x y m i)
                (begin (click x y m i)
                       (helper (+ i 1) m))))
          (if (= j no-of-lifts) (helper 1 j)
              (begin (helper 1 j)
                     (mhelper (+ j 1)))))
        
        (if (click-event mouse-event)
            (begin (mhelper 1)
                   (outside-button 1))
            (set! extra 4))))
    
    (super-new [parent frame])))


;;canvas
(define lift-canvas (new (canvas-key frame)
                         [paint-callback
                          (lambda (canvas dc)
                            (send dc set-pen "grey" 0.25 'hilite)
                            (send dc draw-rectangle 0 0 1350 700)
                            (set-inner-colors)
                            (grid no-of-floors no-of-lifts)
                            (out-grid no-of-floors no-of-lifts)
                            (all-liftgrid no-of-floors no-of-lifts)
                            (showpath)
                            )]))

(define dc (send lift-canvas get-dc))



(void
 (thread (lambda ()
           (let loop ()
             (set-up-colors)
             (set-down-colors)
             (set-inner-colors)
             (set-directions)
             (next)
             (set-positions)
             (send lift-canvas refresh)
             (sleep time-interval)
             (loop)))))