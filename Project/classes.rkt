#lang racket

(require "declarations.rkt")
(provide list-of-lifts list-of-ups list-of-downs next show)

;;class for up button on floors
(define up-button%
  (class object%
    (init-field floor)
    (field [pressed? #f])
    (super-new)
    
    (define/public (press)
      (cond [(lift-at-floor? floor) (begin (set! pressed? #f)
                                           (increase-hold (lifts-at-floor floor)))]
            [else (let ([ref (lift-checker floor 'up (list-shortener list-of-lifts no-of-lifts))]) 
                   (send (list-ref list-of-lifts ref) add-to-destination-list! floor 'up)
                    (set! pressed? #t))]))
    
    (define/public (get-pressed?)
      pressed?)

    (define/public (done)
      (set! pressed? #f))
    ))


;;class for down button on floors
(define down-button%
  (class object%
    (init-field floor)
    (field [pressed? #f])
    (super-new)
    
    (define/public (press)
      (cond [(lift-at-floor? floor) (begin (set! pressed? #f)
                                           (increase-hold (lifts-at-floor floor)))]
            [else (let ([ref (lift-checker floor 'down (list-shortener list-of-lifts no-of-lifts))]) 
                   (send (list-ref list-of-lifts ref) add-to-destination-list! floor 'down)
                    (set! pressed? #t))]))

    (define/public (get-pressed?)
      pressed?)

    (define/public (done)
      (set! pressed? #f))
    ))


;;Lift-positions

(define (lift-positions)
  (define (help i)
    (if (= i 0) (list (send (list-ref list-of-lifts i) get-position))
        (cons (send (list-ref list-of-lifts i) get-position) (help (- i 1)))))
  (reverse (help (- no-of-lifts 1))))

(define (lifts-at-floor n)
  (define (help i l l1)
    (cond [(= i no-of-lifts) l]
          [(= (car l1) n) (help (+ i 1) (cons (list-ref list-of-lifts i) l) (cdr l1))]
          [(help (+ i 1) l (cdr l1))]))
  (help 0 '() (lift-positions)))

(define (lift-at-floor? n)
  (if (null? (lifts-at-floor n)) #f #t)) 
        
;;Increase Hold

(define (increase-hold l)
  (define (help l1)
    (if (= (length l1) 1) (send (car l1) increase-hold)
        (begin (send (car l1) increase-hold) (help (cdr l1)))))
  (help l))


;;class for lift
(define lift%
  (class object%
    (field [position 1])
    (field [direction 'stop])
    (field [hold 0])
    (field [destination-list '()])
    (field [inner-buttons (make-vector no-of-floors #f)])
    (field [distance 0])
    (super-new)

    (define/public (press i)
      (cond [(not (eq? position i)) (vector-set! inner-buttons (- i 1) #t)])
      (set! destination-list (insert i destination-list position))
      (set! direction (cond [(null? destination-list) 'stop]
                            [(> (car destination-list) position) 'up]
                            [else 'down])))

    (define/public (add-to-destination-list! i dir)
      (set! destination-list (insert-with-dir i dir destination-list position))
      (set! direction (cond [(null? destination-list) 'stop]
                            [(> (car destination-list) position) 'up]
                            [else 'down])))

    (define/public (increase-hold)
      (set! hold (+ hold stop-time)))

    (define/public (get-position)
      position)
    (define/public (get-direction)
      direction)
    (define/public (get-hold)
      hold)
    (define/public (get-destination-list)
      destination-list)
    (define/public (get-inner-button i)
      (vector-ref inner-buttons (- i 1)))
    (define/public (get-distance)
      distance)
    
    (define/public (move)
      (cond [(> hold 0) (set! hold (- hold time-interval))]
            [(null? destination-list) (set! direction 'stop)]
            [(= time-interval (* move-time (abs (- position (car destination-list))))) (begin (set! position (car destination-list))
                                                                                              (set! hold stop-time)
                                                                                              (set! destination-list (cdr destination-list))
                                                                                              (set! direction (cond [(null? destination-list) 'stop]
                                                                                                                    [(> (car destination-list) position) 'up]
                                                                                                                    [else 'down]))
                                                                                              (begin (send (list-ref list-of-ups (- position 1)) done)
                                                                                                     (send (list-ref list-of-downs (- position 1)) done))
                                                                                              (vector-set! inner-buttons (- position 1) #f)
                                                                                              )]
            [else (set! position ((if (eq? direction 'up) + -) position (/ time-interval move-time)))]))
     (define/public (move-with-distance)
      (cond [(> hold 0) (set! hold (- hold time-interval))]
            [(null? destination-list) (set! direction 'stop)]
            [(= time-interval (* move-time (abs (- position (car destination-list))))) (begin (set! position (car destination-list))
                                                                                              (set! distance (+ distance (/ time-interval move-time)))
                                                                                              (set! hold stop-time)
                                                                                              (set! destination-list (cdr destination-list))
                                                                                              (set! direction (cond [(null? destination-list) 'stop]
                                                                                                                    [(> (car destination-list) position) 'up]
                                                                                                                    [else 'down]))
                                                                                              (begin (send (list-ref list-of-ups (- position 1)) done)
                                                                                                     (send (list-ref list-of-downs (- position 1)) done))
                                                                                              (vector-set! inner-buttons (- position 1) #f)
                                                                                              )]
            [else (begin (set! position ((if (eq? direction 'up) + -) position (/ time-interval move-time)))
                         (set! distance (+ distance (/ time-interval move-time))))]))
    ))



;;insert function
(define (between? x a b)
  (and (> x (min a b)) (< x (max a b))))

(define (present? x list)
  (cond [(null? list) #f]
        [(= (car list) x) #t]
        [else (present? x (cdr list))]))

(define (insert i list pos)
  (cond [(or (= i pos) (present? i list)) list]
        [(null? list) (cons i list)]
        [(between? i pos (car list)) (cons i list)]
        [else (cons (car list) (insert i (cdr list) (car list)))]))


;;insert-with-dir
(define (same? dir now next)
  (cond [(and (eq? dir 'up) (> next now)) #t]
        [(and (eq? dir 'down) (< next now)) #t]
        [else #f]))
  
(define (insert-with-dir i dir list pos)
  (cond [(and (= i pos) (or (null? list) (same? dir pos (car list)))) list]
        [(null? list) (cons i list)]
        [(and (= i (car list)) (or (null? (cdr list)) (same? dir (car list) (cadr list)))) list]
        [(and (between? i pos (car list)) (same? dir pos (car list))) (cons i list)]
        [else (cons (car list) (insert-with-dir i dir (cdr list) (car list)))]))


;;checker to calculate which lift to call if outer button is pressed
(define (time-calc i dir list pos hold)
  (define new-list (insert-with-dir i dir list pos))
  (define (time i list pos ans)
    (cond [(= i pos) ans]
          [else (time i (cdr list) (car list) (+ ans (* move-time (abs (- (car list) pos))) stop-time))]))
  (if (= i pos) 0
      (- (time i new-list pos hold) stop-time)))

(define (min-finder l)
  (define (min-l l)
    (if (= (length l) 1) (car l)
        (min (car l) (min-l (cdr l)))))
  (define minimum (min-l l))
  (define (helper i)
    (if (= (list-ref l i) minimum) i
        (helper (+ i 1))))
  (helper 0))

(define (lift-checker i dir list-of-lifts)
  (define times (map (lambda (lift) (time-calc i dir (send lift get-destination-list) (send lift get-position) (send lift get-hold))) list-of-lifts))
  (min-finder times))

(define (list-shortener l n)
  (define (helper l i ans)
    (if (= i n) ans
        (helper (cdr l) (+ i 1) (cons (car l) ans))))
  (reverse (helper l 0 '())))


;;lifts
(define (list-of-lifts-maker n ans)
  (if (= n 0) ans
      (list-of-lifts-maker (- n 1) (cons (new lift%) ans))))

(define list-of-lifts
  (list-of-lifts-maker no-of-lifts '()))


;;up buttons
(define (list-of-ups-maker n ans)
  (if (= n 0) ans
      (list-of-ups-maker (- n 1) (cons (new up-button% [floor n]) ans))))

(define list-of-ups
  (list-of-ups-maker no-of-floors '()))


;;down buttons
(define (list-of-downs-maker n ans)
  (if (= n 0) ans
      (list-of-downs-maker (- n 1) (cons (new down-button% [floor n]) ans))))

(define list-of-downs
  (list-of-downs-maker no-of-floors '()))


;;testing
(define (show)
  (define (helper i)
    (if (> i no-of-lifts) (newline)
        (begin (display "Lift ") (display i) (display " :-   Position : ") (display (* 1.0 (send (list-ref list-of-lifts (- i 1)) get-position)))
               (display ",        Direction : ") (display (send (list-ref list-of-lifts (- i 1)) get-direction))
               (display ",        Hold : ") (display (send (list-ref list-of-lifts (- i 1)) get-hold))
               (display ",        Plan : ") (display (send (list-ref list-of-lifts (- i 1)) get-destination-list))
               (display ",        Distance : ") (display (send (list-ref list-of-lifts (- i 1)) get-distance))
               (newline)
               (helper (+ i 1)))))
  (helper 1))

(define (next)
  (define (helper i)
    (if (= i no-of-lifts) (send (list-ref list-of-lifts (- i 1)) move-with-distance)
        (begin (send (list-ref list-of-lifts (- i 1)) move-with-distance)
               (helper (+ i 1)))))
  (helper 1))