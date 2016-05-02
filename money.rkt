#lang racket
(require racket/gui/base)

;functions that other files can access
(provide player-money cpu1-money cpu2-money cpu3-money)

;*************** Money object **************************

(define money% (class object%
  (init amount) ;initialization argument
 
  (define current-balance amount)
 
  (super-new) ;superclass initialization
 
  (define/public (get-balance)
    current-balance)
 
  (define/public (add amount) ;metod for adding money
    (set! current-balance (+ amount current-balance)))
 
  (define/public (sub amount) ;method for subtracting money
     (set! current-balance (- amount current-balance)))
  
   (define/public (set-balance amount) ;method for subtracting money
     (set! current-balance amount))))

;**************** Players money **********************

(define player-money (new money% (amount 1000)))

(define cpu1-money (new money% (amount 1000)))

(define cpu2-money (new money% (amount 1000)))

(define cpu3-money (new money% (amount 1000)))

