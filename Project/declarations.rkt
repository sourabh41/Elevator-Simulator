#lang racket

(provide move-time stop-time time-interval
         no-of-lifts no-of-floors set-lifts set-floors)


;;declarations
(define move-time 2)
(define stop-time 3)
(define time-interval (/ 1 25))
(define no-of-lifts 25)
(define no-of-floors 50)


(define (set-lifts n)
  (set! no-of-lifts n))

(define (set-floors n)
  (set! no-of-floors n))