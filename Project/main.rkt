#lang racket

(require racket/gui)
(require "declarations.rkt")
(require "GUI.rkt")
(require "automator.rkt")
(require "classes.rkt")

(define front-page (new frame%
                        [label "Elevator Simulation"]
                        [width 350]
                        [height 200]))


(send front-page show #t)

(define no-of-floors (new text-field%
                          [label "No. of floors : "]
                          [parent front-page]
                          ))

(define no-of-lift (new text-field%
                        [label "No. of lifts :     "]
                        [parent front-page]
                        ))


(define manual-button (new button%
                           [parent front-page]
                           [label "Manual Simulation"]
                           [callback
                            (lambda (button event)
                              (cond [(not (or (eq? (send no-of-lift get-value) "") (eq? (send no-of-floors get-value)  "")))
                                     (begin (set-lifts (string->number (send no-of-lift get-value)))
                                            (set-floors (string->number (send no-of-floors get-value)))
                                            (h))])
                              )]))

(define (h)
  (send front-page show #f)
  (send frame show #t))


(define automatic-button (new button%
                              [parent front-page]
                              [label "Automatic Simulation"]
                              [callback
                               (lambda (button event)
                                 (cond [(not (or (eq? (send no-of-lift get-value) "") (eq? (send no-of-floors get-value)  "")))
                                        (begin (set-lifts (string->number (send no-of-lift get-value)))
                                               (set-floors (string->number (send no-of-floors get-value)))
                                               (send msg set-label (list-string (distances)))
                                               (send front-page show #f)
                                               (send auto-page show #t))]))
                               ]))


(define (list-string l)
  (define (helper l ans)
    (if (null? l) ans
        (helper (cdr l) (string-append (number->string (car l)) "     " ans))))
  (helper (reverse l) ""))

(define auto-page (new frame%
                       [label "Automatic Simulation"]
                       [width 300]
                       [height 100]))

(define msg (new message%
                 [parent auto-page]
                 [label (make-string (* no-of-lifts 7) #\space)]))