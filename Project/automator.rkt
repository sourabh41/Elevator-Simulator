#lang racket

(provide distances)
(require "declarations.rkt")
(require "classes.rkt")

(define time 50)

(define (up-arrow-press)
  (send (list-ref list-of-ups (random no-of-floors)) press))

(define (down-arrow-press)
  (send (list-ref list-of-downs (random no-of-floors)) press))

(define (lift-button-press)
  (send (list-ref list-of-lifts (random no-of-lifts)) press (+ (random (- no-of-floors 1)) 1)))

(define (get-distances)
  (define (help i l)
    (cond [(= i (- no-of-lifts 1)) (reverse (cons (send (list-ref list-of-lifts i) get-distance) l))]
          [else (help (+ i 1) (cons (send (list-ref list-of-lifts i) get-distance) l))]))
  (help 0 '()))


(define (multi-next n)
  (define (help i)
    (if (= i n) (next)
        (begin (next) (help (+ i 1)))))
  (help 1))

(define (simulator time)
  (define (help i)
    (if (= i time) (get-distances)
        (begin (up-arrow-press)
               (down-arrow-press)
               (multi-next (* 100 move-time))
               (help (+ i 1)))))
  (help 0))

(define (distances)
  (simulator time))

