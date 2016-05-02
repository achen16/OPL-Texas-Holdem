#lang racket
(require games/cards)
(require "money.rkt") ;to check if players have money when setting up the game
(require "bet.rkt")

;provide makes these defines accessable from another file (gui.racket)

(provide deck your-hand cpu1-hand cpu2-hand cpu3-hand mid-cards your-copy 
         cpu1-copy cpu2-copy cpu3-copy points set-up-game who-wins
         player-in-game cpu1-in-game cpu2-in-game cpu3-in-game cpu-decide
         set-turn turn fold-player reset-player-statuses what-did-winner-have? your-copy)


;******************** HAND AND DECK ******************************
;make deck
(define deck '())

;your hand
(define your-hand '())
(define your-copy '())
 

;cpu hand
(define cpu1-hand '())
(define cpu2-hand '())
(define cpu3-hand '())

(define cpu1-copy '())
(define cpu2-copy '())
(define cpu3-copy '())

;define cards in middle
(define mid-cards '())

;set to false when player is out of money or has folded
(define player-in-game #t)
(define cpu1-in-game #t)
(define cpu2-in-game #t)
(define cpu3-in-game #t)

(define fold-player (lambda()
  (set! player-in-game #f)))

(define reset-player-statuses (lambda () 
                                (begin 
                                  (set! player-in-game #t)
                                  (set! cpu1-in-game #t)
                                  (set! cpu2-in-game #t)
                                  (set! cpu3-in-game #t))))

;turn
(define turn 0)
(define set-turn (lambda (x) (set! turn x)))

;****************** FUNCTIONS FOR DRAWING CARDS *******************

;(let (your-first-card-value (send (first your-hand) get-value)))
;(let (your-second-card-value (send (second your-hand) get-value)))
;(let (your-third-card-value (send (third your-hand) get-value)))
;(let (your-fourth-card-value (send (fourth your-hand) get-value)))
;(let (your-fifth-card-value (send (last your-hand) get-value)))

;** if its possible we should have just have one generic function that you pass it
;** a list (hand or middle cards) and it adds a card
;** rather than 4 functions to do it to specific objects
;** we can worry about this later

;draws a card from the deck to your hand
(define draw-to-your-hand (lambda (dk hd)
  (begin (set! your-hand (append hd (list (first dk))))
         (set! deck (cdr dk)))))

;draws a card from the deck to cpu hand
(define draw-to-cpu1-hand (lambda (dk hd)
    (begin (set! cpu1-hand (append hd (list (first dk))))
           (set! deck (cdr dk)))))

;draws a card from the deck to cpu hand
(define draw-to-cpu2-hand (lambda (dk hd)
    (begin (set! cpu2-hand (append hd (list (first dk))))
           (set! deck (cdr dk)))))

;draws a card from the deck to cpu hand
(define draw-to-cpu3-hand (lambda (dk hd)
    (begin (set! cpu3-hand (append hd (list (first dk))))
           (set! deck (cdr dk)))))

;adds a card from the deck to the middle
(define add-middle-card (lambda(dk md)
  (begin (set! mid-cards (append md (list (first dk))))
         (set! deck (cdr dk)))))



; get number values of cards
(define (1v hd)
  (send (first hd) get-value))

(define (2v hd)
  (send (second hd) get-value))

(define (3v hd)
  (send (third hd) get-value))

(define (4v hd)
  (send (fourth hd) get-value))

(define (5v hd)
  (send (fifth hd) get-value))

(define (6v hd)
  (send (sixth hd) get-value))

(define (7v hd)
  (send (seventh hd) get-value))

(define (lastv hd)
  (send (last hd) get-value))


;get suits of cards
(define (1s hd)
  (send (first hd) get-suit))

(define (2s hd)
  (send (second hd) get-suit))

(define (3s hd)
  (send (third hd) get-suit))

(define (4s hd)
  (send (fourth hd) get-suit))

(define (5s hd)
  (send (fifth hd) get-suit))

(define (6s hd)
  (send (sixth hd) get-suit))

(define (7s hd)
  (send (seventh hd) get-suit))



;makes a list of 7 cards, hand and middle cards. For some reason, using a generic function for all hands doesn't add the middle cards 
(define (new-your-hand)
  (begin
    (set! your-copy (append your-copy (list (first your-hand))))
    (set! your-copy (append your-copy (list (second your-hand))))
    (set! your-copy (append your-copy (list (first mid-cards))))
    (set! your-copy (append your-copy (list (second mid-cards))))
    (set! your-copy (append your-copy (list (third mid-cards))))
    (set! your-copy (append your-copy (list (fourth mid-cards))))
    (set! your-copy (append your-copy (list (last mid-cards))))))

(define (new-cpu1-hand)
  (begin
    (set! cpu1-copy (append cpu1-copy (list (first cpu1-hand))))
    (set! cpu1-copy (append cpu1-copy (list (second cpu1-hand))))
    (set! cpu1-copy (append cpu1-copy (list (first mid-cards))))
    (set! cpu1-copy (append cpu1-copy (list (second mid-cards))))
    (set! cpu1-copy (append cpu1-copy (list (third mid-cards))))
    (set! cpu1-copy (append cpu1-copy (list (fourth mid-cards))))
    (set! cpu1-copy (append cpu1-copy (list (last mid-cards))))))

(define (new-cpu2-hand)
  (begin
    (set! cpu2-copy (append cpu2-copy (list (first cpu2-hand))))
    (set! cpu2-copy (append cpu2-copy (list (second cpu2-hand))))
    (set! cpu2-copy (append cpu2-copy (list (first mid-cards))))
    (set! cpu2-copy (append cpu2-copy (list (second mid-cards))))
    (set! cpu2-copy (append cpu2-copy (list (third mid-cards))))
    (set! cpu2-copy (append cpu2-copy (list (fourth mid-cards))))
    (set! cpu2-copy (append cpu2-copy (list (last mid-cards))))))

(define (new-cpu3-hand)
  (begin
    (set! cpu3-copy (append cpu3-copy (list (first cpu3-hand))))
    (set! cpu3-copy (append cpu3-copy (list (second cpu3-hand))))
    (set! cpu3-copy (append cpu3-copy (list (first mid-cards))))
    (set! cpu3-copy (append cpu3-copy (list (second mid-cards))))
    (set! cpu3-copy (append cpu3-copy (list (third mid-cards))))
    (set! cpu3-copy (append cpu3-copy (list (fourth mid-cards))))
    (set! cpu3-copy (append cpu3-copy (list (last mid-cards))))))


;******************** CPU LOGIC *****************************
;currently matches bet everytime,

;checks
;they can call (check 0) to match bet

;raises
; (raise 0 (+ current-bet 10)) to raise bet for cpu1 by 10
; (send cpu1-money get-balance) is money cpu1 has so dont raise over that

;folds
; (set! cpu1-in-game #f) folds

(define cpu-decide (lambda (h)
                     (cond
                       ;no cards turned over always check
                       [(= turn 0) (begin  
                                     (cond
                                       ;cpu1
                                       ((and (eq? h cpu1-hand)(< (random 100) 40)) (check 1)) ;40 percent chance of check
                                       ((and (eq? h cpu1-hand) (< (random 100) 2) (> (send cpu1-money get-balance) current-bet)) ;if cpu has money 2% chance of raising 
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu1-hand) (eq? #t (has-pair? cpu1-hand)) (> (send cpu1-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (- (send cpu1-money get-balance) current-bet)))))
                                       ((eq? h cpu1-hand) (check 1)) 
                                       ;cpu2
                                       ((and (eq? h cpu2-hand)(< (random 100) 40)) (check 2)) ;40 percent chance of check
                                       ((and (eq? h cpu2-hand)(< (random 100) 2) (> (send cpu2-money get-balance) current-bet)) ;if cpu has money 2% chance of raising 
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu2-hand) (eq? #t (has-pair? cpu2-hand)) (> (send cpu2-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (- (send cpu2-money get-balance) current-bet)))))
                                       ((eq? h cpu2-hand) (check 2))
                                       ;cpu3
                                       ((and (eq? h cpu3-hand)(< (random 100) 40)) (check 3)) ;40 percent chance of check
                                       ((and (eq? h cpu3-hand)(< (random 100) 2) (> (send cpu3-money get-balance) current-bet)) ;if cpu has money 2% chance of raising 
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu3-hand) (eq? #t (has-pair? cpu3-hand)) (> (send cpu3-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (- (send cpu3-money get-balance) current-bet)))))
                                       ((eq? h cpu3-hand) (check 3))))]
                       ;3 cards in middle
                       [(= turn 1) (begin
                                     (cond   
                                       ;cpu1
                                       ((and (eq? h cpu1-hand)(< (random 100) 5)) (set! cpu1-in-game #f)) ;10 percent chance of fold
                                       ((and (eq? h cpu1-hand)(< (random 100) 10)(> (send cpu1-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 10))))) ;10 percent chance of raise
                                       ;((and (eq? h cpu1-hand)(> (send cpu1-money get-balance) 10) (or (= (1v cpu1-hand) (1v mid-cards))(= (1v cpu1-hand) (2v mid-cards))(= (2v cpu1-hand) (2v mid-cards))(= (1v cpu1-hand) (3v mid-cards))(= (2v cpu1-hand) (1v mid-cards))(= (2v cpu1-hand) (3v mid-cards))(= (1v mid-cards) (2v mid-cards)) (= (2v mid-cards) (3v mid-cards)))) (raise 1 (+ current-bet 10)))
                                       ((eq? h cpu1-hand) (check 1)) 
                                       ;cpu2
                                       ((and (eq? h cpu2-hand)(< (random 100) 10)) (set! cpu2-in-game #f)) ;10 percent chance of fold
                                       ((and (eq? h cpu2-hand)(< (random 100) 10) (> (send cpu2-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 10))))) ;10 percent chance of raise
                                       ;((and (eq? h cpu2-hand)(> (send cpu2-money get-balance) 10) (or (= (1v cpu2-hand) (1v mid-cards))(= (1v cpu2-hand) (2v mid-cards))(= (2v cpu2-hand) (2v mid-cards))(= (1v cpu2-hand) (3v mid-cards))(= (2v cpu2-hand) (1v mid-cards))(= (2v cpu2-hand) (3v mid-cards))(= (1v mid-cards) (2v mid-cards)) (= (2v mid-cards) (3v mid-cards)))) (raise 2 (+ current-bet 10))) 
                                       ((eq? h cpu2-hand) (check 2))
                                       ;cpu3
                                       ((and (eq? h cpu3-hand)(< (random 100) 10)) (set! cpu3-in-game #f)) ;10 percent chance of fold
                                       ((and (eq? h cpu3-hand)(< (random 100) 10) (> (send cpu3-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 10))))) ;10 percent chance of raise
                                       ;((and (eq? h cpu3-hand)(> (send cpu3-money get-balance) 10) (or (= (1v cpu3-hand) (1v mid-cards))(= (1v cpu3-hand) (2v mid-cards))(= (2v cpu3-hand) (2v mid-cards))(= (1v cpu3-hand) (3v mid-cards))(= (2v cpu3-hand) (1v mid-cards))(= (2v cpu3-hand) (3v mid-cards))(= (1v mid-cards) (2v mid-cards)) (= (2v mid-cards) (3v mid-cards)))) (raise 3 (+ current-bet 10)))
                                       ((eq? h cpu3-hand) (check 3))))]
                       ;4 cards
                       [(= turn 2) (begin 
                                     (cond 
                                       ;cpu1 
                                       ((and (eq? h cpu1-hand)(< (random 100) 5)) (set! cpu1-in-game #f)) ;10 percent chance of fold
                                       ((and (eq? h cpu1-hand)(< (random 100) 10)(> (send cpu1-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 1 (+ current-bet (random (- (send cpu1-money get-balance) current-bet))))) ;10 percent chance of raise
                                       ((and (eq? h cpu1-hand) (eq? #t (has-pair? cpu1-hand)) (= (1v mid-cards) (2v mid-cards)) (> (send cpu1-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 20)))))
                                       ((and (eq? h cpu1-hand) (eq? #t (has-pair? cpu1-hand)) (= (2v mid-cards) (3v mid-cards)) (> (send cpu1-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu1-hand) (eq? #t (has-pair? cpu1-hand)) (= (3v mid-cards) (4v mid-cards)) (> (send cpu1-money get-balance) current-bet))
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 20)))))
                                       ((eq? h cpu1-hand) (check 1))
                                       ;cpu2
                                       ((and (eq? h cpu2-hand)(< (random 100) 5)) (set! cpu2-in-game #f)) ;10% chance of fold
                                       ((and (eq? h cpu2-hand)(< (random 100) 10) (> (send cpu2-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 2 (+ current-bet (random (- (send cpu2-money get-balance) current-bet))))) ;10 percent chance of raise
                                       ((and (eq? h cpu2-hand) (eq? #t (has-pair? cpu2-hand)) (= (1v mid-cards) (2v mid-cards)) (> (send cpu2-money get-balance) current-bet))
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 20)))))
                                       ((and (eq? h cpu2-hand) (eq? #t (has-pair? cpu2-hand)) (= (2v mid-cards) (3v mid-cards)) (> (send cpu2-money get-balance) current-bet))
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu2-hand) (eq? #t (has-pair? cpu2-hand)) (= (3v mid-cards) (4v mid-cards)) (> (send cpu2-money get-balance) current-bet))
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 20))))) 
                                       ((eq? h cpu2-hand) (check 2))
                                       ;cpu3
                                       ((and (eq? h cpu3-hand)(< (random 100) 5)) (set! cpu3-in-game #f)) ;10 percent chance of fold
                                       ((and (eq? h cpu3-hand)(< (random 100) 10) (> (send cpu3-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu3-hand) (eq? #t (has-pair? cpu3-hand)) (= (1v mid-cards) (2v mid-cards)) (> (send cpu3-money get-balance) current-bet))
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu3-hand) (eq? #t (has-pair? cpu3-hand)) (= (2v mid-cards) (3v mid-cards)) (> (send cpu3-money get-balance) current-bet))
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 20))))) 
                                       ((and (eq? h cpu3-hand) (eq? #t (has-pair? cpu3-hand)) (= (3v mid-cards) (4v mid-cards)) (> (send cpu3-money get-balance) current-bet))
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 20))))) 
                                       ((eq? h cpu3-hand) (check 3))))]
                       ;all 5 cards are turned over last chance to check or raise
                        [(= turn 3) (begin 
                                     (cond  
                                       ;cpu1
                                       ((and (eq? h cpu1-hand)(< (random 100) 2)) (set! cpu1-in-game #f)) ;2 percent chance of fold
                                       ((and (eq? h cpu1-hand)(< (random 100) 40)) (check 1))
                                       ((and (eq? h cpu1-hand)(< (random 100) 10) (> (send cpu1-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 10)))))
                                       ((and (eq? h cpu1-hand) (= 0 (points cpu1-copy))) (set! cpu1-in-game #f)) ;no points fold
                                       ((and (eq? h cpu1-hand) (> 4 (points cpu1-copy)) (> (send cpu1-money get-balance) current-bet)) ;over 4 points raise
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 10)))))
                                       ((and (eq? h cpu1-hand) (> 1 (points cpu1-copy)) (> (send cpu1-money get-balance) current-bet)) ;over 1 point raisea 
                                        (raise 1 (+ current-bet (random (quotient (- (send cpu1-money get-balance) current-bet) 10)))))                                  
                                       ((eq? h cpu1-hand) (check 1))
                                       ;cpu2
                                       ((and (eq? h cpu2-hand)(< (random 100) 2)) (set! cpu2-in-game #f)) ;2 percent chance of fold
                                       ((and (eq? h cpu2-hand)(< (random 100) 40)) (check 2))
                                       ((and (eq? h cpu2-hand)(< (random 100) 10) (> (send cpu2-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 10))))) ;10 percent chance of raise
                                       ((and (eq? h cpu2-hand) (= 0 (points cpu2-copy))) (set! cpu2-in-game #f))
                                       ((and (eq? h cpu2-hand) (> 4 (points cpu2-copy)) (> (send cpu2-money get-balance) current-bet)) ;over 4 points raise
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 10)))))  
                                       ((and (eq? h cpu2-hand) (> 1 (points cpu2-copy)) (> (send cpu2-money get-balance) current-bet)) ;over 1 point raise
                                        (raise 2 (+ current-bet (random (quotient (- (send cpu2-money get-balance) current-bet) 10)))))                                     
                                       ((eq? h cpu2-hand) (check 2))
                                       ;cpu3
                                       ((and (eq? h cpu3-hand)(< (random 100) 2)) (set! cpu3-in-game #f)) ;2 percent chance of fold
                                       ((and (eq? h cpu3-hand)(< (random 100) 40)) (check 3))
                                       ((and (eq? h cpu3-hand)(< (random 100) 10) (> (send cpu3-money get-balance) current-bet)) ;if cpu has money 10% chance of raising 
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 10))))) ;10 percent chance of raise
                                       ((and (eq? h cpu3-hand) (= 0 (points cpu3-copy))) (set! cpu3-in-game #f))
                                       ((and (eq? h cpu3-hand) (> 4 (points cpu3-copy)) (> (send cpu3-money get-balance) current-bet)) ;over 4 points raise
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 10))))) 
                                       ((and (eq? h cpu3-hand) (> 1 (points cpu3-copy)) (> (send cpu3-money get-balance) current-bet)) ;over 1 point raise
                                        (raise 3 (+ current-bet (random (quotient (- (send cpu3-money get-balance) current-bet) 10)))))                                
                                       ((eq? h cpu3-hand) (check 3))))])))
                                      
  




;******************* FUNCTIONS FOR LOGIC **************************


;;**LOGIC WILL NOW HAVE TO COMPARE CARDS IN THE HAND AND
;;**THE CARDS IN THE MIDDLE.. NUM OF CARDS IN THE MID DEPENDS ON 
;;**TURN (1st turn 3 cards, 2nd turn 4 cards, 3rd turn 5 cards



;1 point  

(define (has-pair? hd)
  (cond ((null? (cdr hd)) #f)
        ((= (send (car hd) get-value) (send (car(cdr hd)) get-value)) #t)
        (else (has-pair? (cdr hd)))))

(define (pair-value hd)
  (cond ((null? (cdr hd)) 0)
        ((= (send (car hd) get-value) (send (car(cdr hd)) get-value)) (send (car hd) get-value))
        (else (pair-value (cdr hd)))))
      
; two points


(define (has-two-pairs? hd)
    (cond ((and (= (4v hd) (5v hd)) (= (6v hd) (7v hd))) #t)
          ((and (= (3v hd) (4v hd)) (= (6v hd) (7v hd))) #t)
          ((and (= (1v hd) (2v hd)) (= (6v hd) (7v hd))) #t)
          ((and (= (2v hd) (3v hd)) (= (6v hd) (7v hd))) #t)
          ((and (= (3v hd) (4v hd)) (= (5v hd) (6v hd))) #t)
          ((and (= (2v hd) (3v hd)) (= (5v hd) (6v hd))) #t)
          ((and (= (1v hd) (2v hd)) (= (5v hd) (6v hd))) #t)
          ((and (= (2v hd) (3v hd)) (= (4v hd) (5v hd))) #t) 
          ((and (= (1v hd) (2v hd)) (= (4v hd) (5v hd))) #t)
          ((and (= (1v hd) (2v hd)) (= (3v hd) (4v hd))) #t)
          (else #f)))
          
      

(define (two-pairs-value hd)
    (cond ((and (= (4v hd) (5v hd)) (= (6v hd) (7v hd))) (7v hd))
          ((and (= (3v hd) (4v hd)) (= (6v hd) (7v hd))) (7v hd))
          ((and (= (1v hd) (2v hd)) (= (6v hd) (7v hd))) (7v hd))
          ((and (= (2v hd) (3v hd)) (= (6v hd) (7v hd))) (7v hd))
          ((and (= (3v hd) (4v hd)) (= (5v hd) (6v hd))) (6v hd))
          ((and (= (2v hd) (3v hd)) (= (5v hd) (6v hd))) (6v hd))
          ((and (= (1v hd) (2v hd)) (= (5v hd) (6v hd))) (6v hd))
          ((and (= (2v hd) (3v hd)) (= (4v hd) (5v hd))) (5v hd)) 
          ((and (= (1v hd) (2v hd)) (= (4v hd) (5v hd))) (5v hd))
          ((and (= (1v hd) (2v hd)) (= (3v hd) (4v hd))) (4v hd))
          (else 0)))

; three of a kind 3 3 points
(define (has-three-of-a-kind? hd)
  (cond ((null? (cdr(cdr hd))) #f)
        ((= (send (car hd) get-value) (send (car(cdr hd)) get-value) (send (car(cdr(cdr hd))) get-value)) #t)
        (else (has-three-of-a-kind? (cdr hd)))))

(define (three-of-a-kind-value hd)
  (cond ((null? (cdr(cdr hd))) 0)
        ((= (send (car hd) get-value) (send (car(cdr hd)) get-value) (send (car(cdr(cdr hd))) get-value)) (send (car hd) get-value) )
        (else (three-of-a-kind-value (cdr hd)))))
      

;3 points
(define (has-straight? hd)
  (cond ((and (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1)) (= (6v hd) (- (7v hd) 1))) #t)
        ((and (= (2v hd) (- (3v hd) 1)) (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1))) #t)
        ((and (= (1v hd) (- (2v hd) 1)) (= (2v hd) (- (3v hd) 1)) (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1))) #t)
        (else #f)))

(define (straight-value hd)
  (cond ((and (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1)) (= (6v hd) (- (7v hd) 1))) (7v hd))
        ((and (= (2v hd) (- (3v hd) 1)) (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1))) (6v hd))
        (else (5v hd))))

 ;mid-cards is cards in middle, append cards in middle to hand and then logic
  ;will be the same
;4 points
(define (has-flush? hd)
  (cond ((and (eqv? (1s mid-cards) (2s mid-cards)) (eqv? (1s mid-cards)(3s mid-cards)) (eqv? (1s mid-cards)(4s mid-cards)) (eqv? (1s mid-cards)(5s mid-cards))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(6s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(7s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(5s hd)) (eqv? (1s hd)(6s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(6s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(7s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (6s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (7s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(7s hd)) (eqv? (1s hd)(5s hd))) #t)
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(6s hd)) (eqv? (1s hd)(7s hd))) #t)
        ((and (eqv? (2s hd) (3s hd)) (eqv? (2s hd)(4s hd)) (eqv? (2s hd)(5s hd)) (eqv? (2s hd)(6s hd))) #t)
        ((and (eqv? (3s hd) (4s hd)) (eqv? (3s hd)(5s hd)) (eqv? (3s hd)(6s hd)) (eqv? (3s hd)(7s hd))) #t)
        (else #f)))


(define (flush-value hd)
  (cond ((and (eqv? (1s mid-cards) (2s mid-cards)) (eqv? (1s mid-cards)(3s mid-cards)) (eqv? (1s mid-cards)(4s mid-cards)) (eqv? (1s mid-cards)(5s mid-cards))) (1v mid-cards))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(6s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(7s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(5s hd)) (eqv? (1s hd)(6s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(6s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(7s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (6s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (7s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(4s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(7s hd)) (eqv? (1s hd)(5s hd))) (1v hd))
        ((and (eqv? (1s hd) (2s hd)) (eqv? (1s hd)(3s hd)) (eqv? (1s hd)(6s hd)) (eqv? (1s hd)(7s hd))) (1v hd))
        ((and (eqv? (2s hd) (3s hd)) (eqv? (2s hd)(4s hd)) (eqv? (2s hd)(5s hd)) (eqv? (2s hd)(6s hd))) (2v hd))
        ((and (eqv? (3s hd) (4s hd)) (eqv? (3s hd)(5s hd)) (eqv? (3s hd)(6s hd)) (eqv? (3s hd)(7s hd))) (3v hd))
        (else 0)))

 ;mid-cards is cards in middle, append cards in middle to hand and then logic
  ;will be the same
;5 points - I added this, is it necessary?
(define (has-straight-flush? hd)
    (cond ((and (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1)) (= (6v hd) (- (7v hd) 1))
                (eq? (3s hd) (4s hd)) (eq? (3s hd) (5s hd)) (eq? (3s hd) (6s hd)) (eq? (3s hd) (7s hd))) #t)
          ((and (= (2v hd) (- (3v hd) 1)) (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1)) (= (5v hd) (- (6v hd) 1))
                (eq? (2s hd) (3s hd)) (eq? (2s hd) (4s hd)) (eq? (2s hd) (5s hd)) (eq? (2s hd) (6s hd))) #t)
          ((and (= (1v hd) (- (2v hd) 1)) (= (2v hd) (- (3v hd) 1)) (= (3v hd) (- (4v hd) 1)) (= (4v hd) (- (5v hd) 1))
                (eq? (1s hd) (2s hd)) (eq? (1s hd) (3s hd)) (eq? (1s hd) (4s hd)) (eq? (1s hd) (5s hd))) #t)
        (else #f)))
   

 ;mid-cards is cards in middle, append cards in middle to hand and then logic
  ;will be the same
;6 points
(define (has-royal-flush? hd)
  (cond ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (2v hd)) (= 11 (3v hd)) (= 12 (4v hd)) (= 13 (5v hd))) #t)
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (2v hd)) (= 11 (4v hd)) (= 12 (5v hd)) (= 13 (6v hd))) #t)
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (2v hd)) (= 11 (3v hd)) (= 12 (4v hd)) (= 13 (5v hd))) #t)
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (3v hd)) (= 11 (4v hd)) (= 12 (5v hd)) (= 13 (6v hd))) #t)
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (3v hd)) (= 11 (4v hd)) (= 12 (5v hd)) (= 13 (7v hd))) #t)   
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (3v hd)) (= 11 (4v hd)) (= 12 (6v hd)) (= 13 (7v hd))) #t)
        ((and (has-flush? hd)(= 1 (1v hd)) (= 10 (4v hd)) (= 11 (5v hd)) (= 12 (6v hd)) (= 13 (7v hd))) #t)
        (else #f)))

;call functions above then return points
;if has-royal-flush ret 6
;if has-flush ret 4 ... if nothing ret 0
(define (points hd)
  (cond ((eq? #t (has-royal-flush? hd)) 7)
        ((eq? #t (has-straight-flush? hd)) 6)
        ((eq? #t (has-flush? hd)) 5)
        ((eq? #t (has-straight? hd)) 4)
        ((eq? #t (has-three-of-a-kind? hd)) 3)
        ((eq? #t (has-two-pairs? hd)) 2)
        ((eq? #t (has-pair? hd)) 1)
        (else 0)))

;call points for each hand and compare results
;return #t if they win, false if tie or loss;
(define (better-hand h1 h2)
  (cond ((> (points h1) (points h2)) #t)
        ((eq? #t (hands-tie? h1 h2)) (tie-breaker h1 h2))
        (else #f)))
         
;return #t if tie, #f if false
(define (hands-tie? h1 h2)
  (if (= (points h1) (points h2))
      #t
      #f))

(define compare (lambda (p1 p2)
  (if (> p1 p2)
      #t
      #f)))


(define (tie-breaker h1 h2)
  (cond((= 1 (points h1) (points h2)) (compare(pair-value h1)(pair-value h2)))
       ((= 2 (points h1) (points h2)) (compare (two-pairs-value h1) (two-pairs-value h2)))
       ((= 3 (points h1) (points h2)) (compare (three-of-a-kind-value h1) (three-of-a-kind-value h2)))
       ((= 4 (points h1) (points h2)) (compare(straight-value h1)(straight-value h2)))
       ((= 5 (points h1) (points h2)) (compare(flush-value h1)(flush-value h2)))
       ((= 6 (points h1) (points h2)) (compare(straight-value h1)(straight-value h2)))
       ((= 7 (points h1) (points h2)) #f)
       (else #f)))
  
       




;** This will have to be modified for 4 hands return 1 if player 1 wins
;** return 2 if player 2 wins, 3 if player 3 wins e
(define who-wins (lambda ()
  (cond ((and player-in-game cpu1-in-game cpu2-in-game cpu3-in-game (eqv? #t (better-hand your-copy cpu1-copy)) (eqv? #t (better-hand your-copy cpu2-copy)) (eqv? #t (better-hand your-copy cpu3-copy))) 1)
        ((and player-in-game (not cpu1-in-game) cpu2-in-game cpu3-in-game (eqv? #t (better-hand your-copy cpu2-copy)) (eqv? #t (better-hand your-copy cpu3-copy))) 1)
        ((and player-in-game cpu1-in-game (not cpu2-in-game) cpu3-in-game (eqv? #t (better-hand your-copy cpu1-copy)) (eqv? #t (better-hand your-copy cpu3-copy))) 1)
        ((and player-in-game cpu1-in-game cpu2-in-game (not cpu3-in-game) (eqv? #t (better-hand your-copy cpu1-copy)) (eqv? #t (better-hand your-copy cpu2-copy))) 1)
        ((and player-in-game (not cpu1-in-game) (not cpu2-in-game) cpu3-in-game (eqv? #t (better-hand your-copy cpu3-copy))) 1)
        ((and player-in-game cpu1-in-game (not cpu2-in-game) (not cpu3-in-game) (eqv? #t (better-hand your-copy cpu1-copy))) 1)
        ((and player-in-game (not cpu1-in-game) cpu2-in-game (not cpu3-in-game) (eqv? #t (better-hand your-copy cpu2-copy))) 1)
        ((and player-in-game (not cpu1-in-game) (not cpu2-in-game) (not cpu3-in-game)) 1)
        ((and cpu1-in-game cpu2-in-game cpu3-in-game player-in-game (eqv? #t (better-hand cpu1-copy your-copy)) (eqv? #t (better-hand cpu1-copy cpu2-copy)) (eqv? #t (better-hand cpu1-copy cpu3-copy))) 2)
        ((and cpu1-in-game (not player-in-game) cpu2-in-game cpu3-in-game (eqv? #t (better-hand cpu1-copy cpu2-copy)) (eqv? #t (better-hand cpu1-copy cpu3-copy))) 2)
        ((and cpu1-in-game player-in-game (not cpu2-in-game) cpu3-in-game (eqv? #t (better-hand cpu1-copy your-copy)) (eqv? #t (better-hand cpu1-copy cpu3-copy))) 2)
        ((and cpu1-in-game player-in-game cpu2-in-game (not cpu3-in-game) (eqv? #t (better-hand cpu1-copy your-copy)) (eqv? #t (better-hand cpu1-copy cpu2-copy))) 2)
        ((and cpu1-in-game (not player-in-game) (not cpu2-in-game) cpu3-in-game (eqv? #t (better-hand cpu1-copy cpu3-copy))) 2)
        ((and cpu1-in-game player-in-game (not cpu2-in-game) (not cpu3-in-game) (eqv? #t (better-hand cpu1-copy your-copy))) 2)
        ((and cpu1-in-game (not player-in-game) cpu2-in-game (not cpu3-in-game) (eqv? #t (better-hand cpu1-copy cpu2-copy))) 2)
        ((and cpu1-in-game (not player-in-game) (not cpu2-in-game) (not cpu3-in-game)) 2)
        ((and cpu2-in-game cpu1-in-game cpu3-in-game player-in-game (eqv? #t (better-hand cpu2-copy your-copy)) (eqv? #t (better-hand cpu2-copy cpu1-copy)) (eqv? #t (better-hand cpu2-copy cpu3-copy))) 3)
        ((and cpu2-in-game (not player-in-game) cpu1-in-game cpu3-in-game (eqv? #t (better-hand cpu2-copy cpu1-copy)) (eqv? #t (better-hand cpu2-copy cpu3-copy))) 3)
        ((and cpu2-in-game player-in-game (not cpu1-in-game) cpu3-in-game (eqv? #t (better-hand cpu2-copy your-copy)) (eqv? #t (better-hand cpu2-copy cpu3-copy))) 3)
        ((and cpu2-in-game player-in-game cpu1-in-game (not cpu3-in-game) (eqv? #t (better-hand cpu2-copy your-copy)) (eqv? #t (better-hand cpu2-copy cpu1-copy))) 3)
        ((and cpu2-in-game (not player-in-game) (not cpu1-in-game) cpu3-in-game (eqv? #t (better-hand cpu2-copy cpu3-copy))) 3)
        ((and cpu2-in-game player-in-game (not cpu1-in-game) (not cpu3-in-game) (eqv? #t (better-hand cpu2-copy your-copy))) 3)
        ((and cpu2-in-game (not player-in-game) cpu1-in-game (not cpu3-in-game) (eqv? #t (better-hand cpu2-copy cpu1-copy))) 3)
        ((and cpu2-in-game (not player-in-game) (not cpu1-in-game) (not cpu3-in-game)) 3)
        ((and cpu3-in-game cpu1-in-game cpu2-in-game player-in-game (eqv? #t (better-hand cpu3-copy your-copy)) (eqv? #t (better-hand cpu3-copy cpu1-copy)) (eqv? #t (better-hand cpu3-copy cpu2-copy))) 4)
        ((and cpu3-in-game (not player-in-game) cpu2-in-game cpu1-in-game (eqv? #t (better-hand cpu3-copy cpu2-copy)) (eqv? #t (better-hand cpu3-copy cpu1-copy))) 4)
        ((and cpu3-in-game player-in-game (not cpu2-in-game) cpu1-in-game (eqv? #t (better-hand cpu3-copy your-copy)) (eqv? #t (better-hand cpu3-copy cpu1-copy))) 4)
        ((and cpu3-in-game player-in-game cpu2-in-game (not cpu1-in-game) (eqv? #t (better-hand cpu3-copy your-copy)) (eqv? #t (better-hand cpu3-copy cpu2-copy))) 4)
        ((and cpu3-in-game (not player-in-game) (not cpu2-in-game) cpu1-in-game (eqv? #t (better-hand cpu3-copy cpu1-copy))) 4)
        ((and cpu3-in-game player-in-game (not cpu2-in-game) (not cpu1-in-game) (eqv? #t (better-hand cpu3-copy your-copy))) 4)
        ((and cpu3-in-game (not player-in-game) cpu2-in-game (not cpu1-in-game) (eqv? #t (better-hand cpu3-copy cpu2-copy))) 4)
        ((and cpu3-in-game (not player-in-game) (not cpu2-in-game) (not cpu1-in-game)) 4)
        (else 0))))


(define what-did-winner-have? (lambda() 
  (cond ((= 1 (who-wins)) (points your-copy))
        ((= 2 (who-wins)) (points cpu1-copy))
        ((= 3 (who-wins)) (points cpu2-copy))
        ((= 4 (who-wins)) (points cpu3-copy)))))

        

;********************* Set up game **********************************

(define set-up-game (lambda () 
                      (begin 
                        (set! deck (shuffle-list (make-deck) 20))
                        (set! your-hand '())
                        (set! cpu1-hand '())
                        (set! cpu2-hand '())
                        (set! cpu3-hand '())
                        (set! cpu3-hand '())
                        (set! mid-cards '())
                        (set! your-copy '())
                        (set! cpu1-copy '())
                        (set! cpu2-copy '())
                        (set! cpu3-copy '())
                        ;draw 5 cards to middle
                        (add-middle-card deck mid-cards)
                        (add-middle-card deck mid-cards)
                        (add-middle-card deck mid-cards)
                        (add-middle-card deck mid-cards)
                        (add-middle-card deck mid-cards)
                        ;if player is still in the game draw cards
                        (if (> (send player-money get-balance) 0) 
                            (begin (set! player-in-game #t)
                                   (draw-to-your-hand deck your-hand)
                                   (draw-to-your-hand deck your-hand)
                                   (new-your-hand)
                                   (sort-yh1 your-copy))
                            (set! player-in-game #f))
                        ;if cpu1 is still in the game draw cards
                        (if (> (send cpu1-money get-balance) 0) 
                            (begin (draw-to-cpu1-hand deck cpu1-hand)
                                   (draw-to-cpu1-hand deck cpu1-hand)
                                   (new-cpu1-hand)
                                   (sort-cpu11 cpu1-copy)
                                   (set! cpu1-in-game #t))
                            (set! cpu1-in-game #f))
                         ;if cpu2 is still in the game draw cards
                        (if (> (send cpu2-money get-balance) 0) 
                            (begin (set! cpu2-in-game #t) 
                                   (draw-to-cpu2-hand deck cpu2-hand)
                                   (draw-to-cpu2-hand deck cpu2-hand)
                                   (new-cpu2-hand)
                                   (sort-cpu21 cpu2-copy))  
                            (set! cpu2-in-game #f))
                         ;if cpu3 is still in the game draw cards
                        (if (> (send cpu3-money get-balance) 0) 
                            (begin (draw-to-cpu3-hand deck cpu3-hand)
                                   (draw-to-cpu3-hand deck cpu3-hand)
                                   (set! cpu3-in-game #t)
                                   (new-cpu3-hand)
                                   (sort-cpu31 cpu3-copy)) 
                            (set! cpu3-in-game #f)))))


(define (check-hand hd)
  (list (1v hd) (2v hd) (3v hd) (4v hd) (5v hd) (6v hd) (7v hd)))

(define (1p) (check-hand your-copy))
(define (2p) (check-hand cpu1-copy))
(define (3p) (check-hand cpu2-copy))
(define (4p) (check-hand cpu3-copy))

;***************sorting*****************************************************************************************************************  
(define (sort-yh1 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd)) (<= (1v hd) (7v hd))) 
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh2 your-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)) (<= (2v hd) (7v hd))) 
         (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh2 your-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd)) (<= (3v hd) (7v hd))) 
         (begin (set! your-copy (append your-copy (list (third your-copy)))) 
                (set! your-copy (remove (third your-copy) your-copy))
                (sort-yh2 your-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)) (<= (4v hd) (7v hd))) 
         (begin (set! your-copy (append your-copy (list (fourth your-copy))))
                (set! your-copy (remove (fourth your-copy) your-copy))
                (sort-yh2 your-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd)) (<= (5v hd) (7v hd)))
         (begin (set! your-copy (append your-copy (list (fifth your-copy)))) 
                (set! your-copy (remove (fifth your-copy) your-copy))
                (sort-yh2 your-copy)))
        ((and (<= (6v hd) (1v hd)) (<= (6v hd) (2v hd)) (<= (6v hd) (3v hd)) (<= (6v hd) (4v hd)) (<= (6v hd) (5v hd)) (<= (6v hd) (7v hd))) 
         (begin (set! your-copy (append your-copy (list (sixth your-copy)))) 
                (set! your-copy (remove (sixth your-copy) your-copy))
                (sort-yh2 your-copy)))
   (else (begin (set! your-copy (append your-copy (list (seventh your-copy)))) 
                (set! your-copy (remove (seventh your-copy) your-copy))
                (sort-yh2 your-copy)))))

(define (sort-yh2 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd))) 
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh3 your-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)))
         (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh3 your-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd))) 
         (begin (set! your-copy (append your-copy (list (third your-copy)))) 
                (set! your-copy (remove (third your-copy) your-copy))
                (sort-yh3 your-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)))
         (begin (set! your-copy (append your-copy (list (fourth your-copy))))
                (set! your-copy (remove (fourth your-copy) your-copy))
                (sort-yh3 your-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd))) 
         (begin (set! your-copy (append your-copy (list (fifth your-copy)))) 
                (set! your-copy (remove (fifth your-copy) your-copy))
                (sort-yh3 your-copy)))
   (else (begin (set! your-copy (append your-copy (list (sixth your-copy)))) 
                (set! your-copy (remove (sixth your-copy) your-copy))
                (sort-yh3 your-copy)))))
 
(define (sort-yh3 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)))
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh4 your-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)))
         (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh4 your-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)))
         (begin (set! your-copy (append your-copy (list (third your-copy)))) 
                (set! your-copy (remove (third your-copy) your-copy))
                (sort-yh4 your-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)))
         (begin (set! your-copy (append your-copy (list (fourth your-copy))))
                (set! your-copy (remove (fourth your-copy) your-copy))
                (sort-yh4 your-copy)))
   (else (begin (set! your-copy (append your-copy (list (fifth your-copy)))) 
                (set! your-copy (remove (fifth your-copy) your-copy))
                (sort-yh4 your-copy)))))

(define (sort-yh4 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)))
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh5 your-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)))
         (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh5 your-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)))
         (begin (set! your-copy (append your-copy (list (third your-copy)))) 
                (set! your-copy (remove (third your-copy) your-copy))
                (sort-yh5 your-copy)))
   (else (begin (set! your-copy (append your-copy (list (fourth your-copy)))) 
                (set! your-copy (remove (fourth your-copy) your-copy))
                (sort-yh5 your-copy)))))

(define (sort-yh5 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)))
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh6 your-copy)                ))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)))
         (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh6 your-copy)                ))
   (else (begin (set! your-copy (append your-copy (list (third your-copy)))) 
                (set! your-copy (remove (third your-copy) your-copy))
                (sort-yh6 your-copy)))))
                

(define (sort-yh6 hd) 
  (cond ((<= (1v hd) (2v hd))
         (begin (set! your-copy (append your-copy (list (first your-copy)))) 
                (set! your-copy (remove (first your-copy) your-copy))
                (sort-yh7 your-copy)))
   (else (begin (set! your-copy (append your-copy (list (second your-copy)))) 
                (set! your-copy (remove (second your-copy) your-copy))
                (sort-yh7 your-copy)))))

(define (sort-yh7 hd)
  (begin (set! your-copy (append your-copy (list (first your-copy)))) 
         (set! your-copy (remove (first your-copy) your-copy))))




(define (sort-cpu11 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd)) (<= (1v hd) (7v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)) (<= (2v hd) (7v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd)) (<= (3v hd) (7v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (third cpu1-copy)))) 
                (set! cpu1-copy (remove (third cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)) (<= (4v hd) (7v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (fourth cpu1-copy))))
                (set! cpu1-copy (remove (fourth cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd)) (<= (5v hd) (7v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (fifth cpu1-copy)))) 
                (set! cpu1-copy (remove (fifth cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
        ((and (<= (6v hd) (1v hd)) (<= (6v hd) (2v hd)) (<= (6v hd) (3v hd)) (<= (6v hd) (4v hd)) (<= (6v hd) (5v hd)) (<= (6v hd) (7v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (sixth cpu1-copy)))) 
                (set! cpu1-copy (remove (sixth cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (seventh cpu1-copy)))) 
                (set! cpu1-copy (remove (seventh cpu1-copy) cpu1-copy))
                (sort-cpu12 cpu1-copy)))))

(define (sort-cpu12 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (third cpu1-copy)))) 
                (set! cpu1-copy (remove (third cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (fourth cpu1-copy))))
                (set! cpu1-copy (remove (fourth cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd))) 
         (begin (set! cpu1-copy (append cpu1-copy (list (fifth cpu1-copy)))) 
                (set! cpu1-copy (remove (fifth cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (sixth cpu1-copy)))) 
                (set! cpu1-copy (remove (sixth cpu1-copy) cpu1-copy))
                (sort-cpu13 cpu1-copy)))))
 
(define (sort-cpu13 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu14 cpu1-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu14 cpu1-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (third cpu1-copy)))) 
                (set! cpu1-copy (remove (third cpu1-copy) cpu1-copy))
                (sort-cpu14 cpu1-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (fourth cpu1-copy))))
                (set! cpu1-copy (remove (fourth cpu1-copy) cpu1-copy))
                (sort-cpu14 cpu1-copy)))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (fifth cpu1-copy)))) 
                (set! cpu1-copy (remove (fifth cpu1-copy) cpu1-copy))
                (sort-cpu14 cpu1-copy)))))

(define (sort-cpu14 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu15 cpu1-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu15 cpu1-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (third cpu1-copy)))) 
                (set! cpu1-copy (remove (third cpu1-copy) cpu1-copy))
                (sort-cpu15 cpu1-copy)))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (fourth cpu1-copy)))) 
                (set! cpu1-copy (remove (fourth cpu1-copy) cpu1-copy))
                (sort-cpu15 cpu1-copy)))))

(define (sort-cpu15 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu16 cpu1-copy)                ))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)))
         (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu16 cpu1-copy)                ))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (third cpu1-copy)))) 
                (set! cpu1-copy (remove (third cpu1-copy) cpu1-copy))
                (sort-cpu16 cpu1-copy)))))
                

(define (sort-cpu16 hd) 
  (cond ((<= (1v hd) (2v hd))
         (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
                (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))
                (sort-cpu17 cpu1-copy)))
   (else (begin (set! cpu1-copy (append cpu1-copy (list (second cpu1-copy)))) 
                (set! cpu1-copy (remove (second cpu1-copy) cpu1-copy))
                (sort-cpu17 cpu1-copy)))))

(define (sort-cpu17 hd)
  (begin (set! cpu1-copy (append cpu1-copy (list (first cpu1-copy)))) 
         (set! cpu1-copy (remove (first cpu1-copy) cpu1-copy))))




(define (sort-cpu21 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd)) (<= (1v hd) (7v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)) (<= (2v hd) (7v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd)) (<= (3v hd) (7v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (third cpu2-copy)))) 
                (set! cpu2-copy (remove (third cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)) (<= (4v hd) (7v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (fourth cpu2-copy))))
                (set! cpu2-copy (remove (fourth cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd)) (<= (5v hd) (7v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (fifth cpu2-copy)))) 
                (set! cpu2-copy (remove (fifth cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
        ((and (<= (6v hd) (1v hd)) (<= (6v hd) (2v hd)) (<= (6v hd) (3v hd)) (<= (6v hd) (4v hd)) (<= (6v hd) (5v hd)) (<= (6v hd) (7v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (sixth cpu2-copy)))) 
                (set! cpu2-copy (remove (sixth cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (seventh cpu2-copy)))) 
                (set! cpu2-copy (remove (seventh cpu2-copy) cpu2-copy))
                (sort-cpu22 cpu2-copy)))))

(define (sort-cpu22 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (third cpu2-copy)))) 
                (set! cpu2-copy (remove (third cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (fourth cpu2-copy))))
                (set! cpu2-copy (remove (fourth cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd))) 
         (begin (set! cpu2-copy (append cpu2-copy (list (fifth cpu2-copy)))) 
                (set! cpu2-copy (remove (fifth cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (sixth cpu2-copy)))) 
                (set! cpu2-copy (remove (sixth cpu2-copy) cpu2-copy))
                (sort-cpu23 cpu2-copy)))))
 
(define (sort-cpu23 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu24 cpu2-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu24 cpu2-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (third cpu2-copy)))) 
                (set! cpu2-copy (remove (third cpu2-copy) cpu2-copy))
                (sort-cpu24 cpu2-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (fourth cpu2-copy))))
                (set! cpu2-copy (remove (fourth cpu2-copy) cpu2-copy))
                (sort-cpu24 cpu2-copy)))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (fifth cpu2-copy)))) 
                (set! cpu2-copy (remove (fifth cpu2-copy) cpu2-copy))
                (sort-cpu24 cpu2-copy)))))

(define (sort-cpu24 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu25 cpu2-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu25 cpu2-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (third cpu2-copy)))) 
                (set! cpu2-copy (remove (third cpu2-copy) cpu2-copy))
                (sort-cpu25 cpu2-copy)))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (fourth cpu2-copy)))) 
                (set! cpu2-copy (remove (fourth cpu2-copy) cpu2-copy))
                (sort-cpu25 cpu2-copy)))))

(define (sort-cpu25 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu26 cpu2-copy)                ))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)))
         (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu26 cpu2-copy)                ))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (third cpu2-copy)))) 
                (set! cpu2-copy (remove (third cpu2-copy) cpu2-copy))
                (sort-cpu26 cpu2-copy)))))
                

(define (sort-cpu26 hd) 
  (cond ((<= (1v hd) (2v hd))
         (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
                (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))
                (sort-cpu27 cpu2-copy)))
   (else (begin (set! cpu2-copy (append cpu2-copy (list (second cpu2-copy)))) 
                (set! cpu2-copy (remove (second cpu2-copy) cpu2-copy))
                (sort-cpu27 cpu2-copy)))))

(define (sort-cpu27 hd)
  (begin (set! cpu2-copy (append cpu2-copy (list (first cpu2-copy)))) 
         (set! cpu2-copy (remove (first cpu2-copy) cpu2-copy))))



(define (sort-cpu31 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd)) (<= (1v hd) (7v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)) (<= (2v hd) (7v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd)) (<= (3v hd) (7v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (third cpu3-copy)))) 
                (set! cpu3-copy (remove (third cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)) (<= (4v hd) (7v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (fourth cpu3-copy))))
                (set! cpu3-copy (remove (fourth cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd)) (<= (5v hd) (7v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (fifth cpu3-copy)))) 
                (set! cpu3-copy (remove (fifth cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
        ((and (<= (6v hd) (1v hd)) (<= (6v hd) (2v hd)) (<= (6v hd) (3v hd)) (<= (6v hd) (4v hd)) (<= (6v hd) (5v hd)) (<= (6v hd) (7v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (sixth cpu3-copy)))) 
                (set! cpu3-copy (remove (sixth cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (seventh cpu3-copy)))) 
                (set! cpu3-copy (remove (seventh cpu3-copy) cpu3-copy))
                (sort-cpu32 cpu3-copy)))))

(define (sort-cpu32 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)) (<= (1v hd) (6v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)) (<= (2v hd) (6v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)) (<= (3v hd) (6v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (third cpu3-copy)))) 
                (set! cpu3-copy (remove (third cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)) (<= (4v hd) (6v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (fourth cpu3-copy))))
                (set! cpu3-copy (remove (fourth cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))
        ((and (<= (5v hd) (1v hd)) (<= (5v hd) (2v hd)) (<= (5v hd) (3v hd)) (<= (5v hd) (4v hd)) (<= (5v hd) (6v hd))) 
         (begin (set! cpu3-copy (append cpu3-copy (list (fifth cpu3-copy)))) 
                (set! cpu3-copy (remove (fifth cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (sixth cpu3-copy)))) 
                (set! cpu3-copy (remove (sixth cpu3-copy) cpu3-copy))
                (sort-cpu33 cpu3-copy)))))
 
(define (sort-cpu33 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)) (<= (1v hd) (5v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu34 cpu3-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)) (<= (2v hd) (5v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu34 cpu3-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)) (<= (3v hd) (5v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (third cpu3-copy)))) 
                (set! cpu3-copy (remove (third cpu3-copy) cpu3-copy))
                (sort-cpu34 cpu3-copy)))
        ((and (<= (4v hd) (1v hd)) (<= (4v hd) (2v hd)) (<= (4v hd) (3v hd)) (<= (4v hd) (5v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (fourth cpu3-copy))))
                (set! cpu3-copy (remove (fourth cpu3-copy) cpu3-copy))
                (sort-cpu34 cpu3-copy)))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (fifth cpu3-copy)))) 
                (set! cpu3-copy (remove (fifth cpu3-copy) cpu3-copy))
                (sort-cpu34 cpu3-copy)))))

(define (sort-cpu34 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)) (<= (1v hd) (4v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu35 cpu3-copy)))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)) (<= (2v hd) (4v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu35 cpu3-copy)))
        ((and (<= (3v hd) (1v hd)) (<= (3v hd) (2v hd)) (<= (3v hd) (4v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (third cpu3-copy)))) 
                (set! cpu3-copy (remove (third cpu3-copy) cpu3-copy))
                (sort-cpu35 cpu3-copy)))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (fourth cpu3-copy)))) 
                (set! cpu3-copy (remove (fourth cpu3-copy) cpu3-copy))
                (sort-cpu35 cpu3-copy)))))

(define (sort-cpu35 hd) 
  (cond ((and (<= (1v hd) (2v hd)) (<= (1v hd) (3v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu36 cpu3-copy)                ))
        ((and (<= (2v hd) (1v hd)) (<= (2v hd) (3v hd)))
         (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu36 cpu3-copy)                ))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (third cpu3-copy)))) 
                (set! cpu3-copy (remove (third cpu3-copy) cpu3-copy))
                (sort-cpu36 cpu3-copy)))))
                

(define (sort-cpu36 hd) 
  (cond ((<= (1v hd) (2v hd))
         (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
                (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))
                (sort-cpu37 cpu3-copy)))
   (else (begin (set! cpu3-copy (append cpu3-copy (list (second cpu3-copy)))) 
                (set! cpu3-copy (remove (second cpu3-copy) cpu3-copy))
                (sort-cpu37 cpu3-copy)))))

(define (sort-cpu37 hd)
  (begin (set! cpu3-copy (append cpu3-copy (list (first cpu3-copy)))) 
         (set! cpu3-copy (remove (first cpu3-copy) cpu3-copy))))



