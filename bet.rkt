#lang racket
(require "money.rkt")

(provide current-bet player-bet cpu1-bet cpu2-bet cpu3-bet
         reset-bets check raise)

(define current-bet 0)
(define player-bet 0)
(define cpu1-bet 0)
(define cpu2-bet 0)
(define cpu3-bet 0)


;************************************************ Betting ***********************************************************************
;match current bet
(define check (lambda (player)
                (cond
                  ((= player 0) (if (> (send player-money get-balance) current-bet) 
                                              (set! player-bet current-bet)  
                                              (set! player-bet (send player-money get-balance))))
                  ((= player 1) (if (> (send cpu1-money get-balance) current-bet) 
                                              (set! cpu1-bet current-bet)  
                                              (set! cpu1-bet (send cpu1-money get-balance))))
                  ((= player 2) (if (> (send cpu2-money get-balance) current-bet) 
                                              (set! cpu2-bet current-bet)  
                                              (set! cpu2-bet (send cpu2-money get-balance))))
                  ((= player 3) (if (> (send cpu3-money get-balance) current-bet) 
                                              (set! cpu3-bet current-bet)  
                                              (set! cpu3-bet (send cpu3-money get-balance)))))))

;raise current bet
(define raise (lambda (player new-bet)
                (begin
                  (set! current-bet new-bet)
                  (cond
                    ((= player 0) (set! player-bet current-bet))
                    ((= player 1) (set! cpu1-bet current-bet))
                    ((= player 2) (set! cpu2-bet current-bet))
                    ((= player 3) (set! cpu3-bet current-bet))))))
                                



;******************* Make Bet *************************
;every round the betting will start over
(define reset-bets (lambda () 
                     (begin
                       (set! player-bet 0)
                       (set! cpu1-bet 0)
                       (set! cpu2-bet 0)
                       (set! cpu3-bet 0)
                       (set! current-bet 0))))






