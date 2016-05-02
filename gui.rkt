#lang racket
(require games/cards)
(require racket/gui/base)
(require "logic.rkt")
(require "money.rkt")
(require "bet.rkt")

;******************* MAKE WINDOW ************************************

;define window (or a table as racket calls it)
(define table(make-table "Bobby Donald and Alex Chen's Poker Game Texas Hold'em" 16 5))

;warning dialog
(define warning (new dialog%	 
                     [label "Warning!"]
                     [min-width 600]
                     [min-height 100]
                     [style '(close-button)]
                     [alignment '(center center)]))

(define warning-label (new message%	 
  [label (format "Current bet is ~a. If you want to raise, you must bet more than that." current-bet)]	 
  [parent warning]
  [auto-resize #t]))


;popup for game-over
(define game-over (new dialog%	 
                     [label "Game-Over"]
                     [min-width 600]
                     [min-height 100]
                     [style '(close-button)]
                     [alignment '(center center)]))

(define game-over-label (new message%	 
  [label "Sorry You Lose"]	 
  [parent game-over]
  [auto-resize #t]))


;***************Money dialog***************************

(define info (new dialog%	 
                     [label "Winner!"]
                     [min-width 600]
                     [min-height 100]
                     [style '(close-button)]
                     [alignment '(center center)]))

(define info-label (new message%	 
  [label ""]	 
  [parent info]
  [auto-resize #t]))

(define win-label (new message%	 
  [label ""]	 
  [parent info]
  [auto-resize #t]))

;display all the cards
(define add-all-cards-to-window (lambda ()
                                  ;every time a card is added to the window the lambda function is called with i incremented by 1 
                                  ;so with 0, 1, 2 ,3 ,4 for the middle cards 0 and 1 for the hands
                                  (begin (cond ( player-in-game (begin 
                                                                  (send table add-cards your-hand 500 350 (lambda (i) (values (* i 80)
                                                                                                                              0)))
                                                                  (send (first your-hand) user-can-move #f)
                                                                  (send (second your-hand) user-can-move #f)
                                                                  (send (first your-hand) snap-back-after-move #t)
                                                                  (send (second your-hand) snap-back-after-move #t)
                                                                  ;make users cards face up
                                                                  (send table cards-face-up your-hand)
                                                                  )))
                                         
                                         (cond ( cpu1-in-game (begin 
                                                                (send table add-cards cpu1-hand 100 100 (lambda (i) (values 0
                                                                                                                            (* i 80))))
                                                                (send table rotate-cards cpu1-hand 90)
                                                                (send (first cpu1-hand) user-can-move #f)
                                                                (send (second cpu1-hand) user-can-move #f)
                                                                (send (first cpu1-hand) snap-back-after-move #t)
                                                                (send (second cpu1-hand) snap-back-after-move #t)
                                                                )))
                                         
                                         (cond (cpu2-in-game (begin 
                                                               (send table add-cards cpu2-hand 500 0 (lambda (i) (values (* i 80) 
                                                                                                                         0)))
                                                               (send (first cpu2-hand) user-can-move #f)
                                                               (send (second cpu2-hand) user-can-move #f)
                                                               (send (first cpu2-hand) snap-back-after-move #t)
                                                               (send (second cpu2-hand) snap-back-after-move #t)
                                                               )))
                                         
                                         (cond (cpu3-in-game (begin 
                                                               (send table add-cards cpu3-hand 1000 100 (lambda (i) (values 0
                                                                                                                            (* i 80))))
                                                               (send table rotate-cards cpu3-hand 90)
                                                               (send (first cpu3-hand) user-can-move #f)
                                                               (send (second cpu3-hand) user-can-move #f)
                                                               (send (first cpu3-hand) snap-back-after-move #t)
                                                               (send (second cpu3-hand) snap-back-after-move #t)
                                                               )))
                                         
                                         (send table add-cards mid-cards 400 200 (lambda (i) (values (* i 80)
                                                                                                     0)))
                                         
                                         (send (first mid-cards) user-can-move #f)
                                         (send (second mid-cards) user-can-move #f)
                                         (send (third mid-cards) user-can-move #f)
                                         (send (fourth mid-cards) user-can-move #f)
                                         (send (fifth mid-cards) user-can-move #f)
                                         
                                         (send (first mid-cards) snap-back-after-move #t)
                                         (send (second mid-cards) snap-back-after-move #t)
                                         (send (third mid-cards) snap-back-after-move #t)
                                         (send (fourth mid-cards) snap-back-after-move #t)
                                         (send (fifth mid-cards) snap-back-after-move #t))))


;*************************** LABELS ON WINDOW *****************************
; Make a static text message in the frame
(define msg (new message% [parent table]
                          [label "Good Luck!"]))

;*************************** MONEY LABLES ******************************************
(define balance-panel (new horizontal-panel%	 
     [parent table]
     [alignment '(center center)]))

(define cpu1-balance (new message% [parent balance-panel]
                          [label (format "CPU1 Balance: ~a " (send cpu1-money get-balance))]
                          [auto-resize #t]))

(define cpu2-balance (new message% [parent balance-panel]
                          [label (format "CPU1 Balance: ~a " (send cpu2-money get-balance))]
                          [auto-resize #t]))

(define cpu3-balance (new message% [parent balance-panel]
                          [label (format "CPU1 Balance: ~a " (send cpu3-money get-balance))]
                          [auto-resize #t]))


;*************************** BET LABLES ******************************************
(define label-panel (new horizontal-panel%	 
     [parent table]
     [alignment '(center center)]))

(define current-bet-label (new message% [parent label-panel]
                          [label (format "Current bet: ~a " current-bet)]
                          (auto-resize #t)))

(define cpu1-bet-label (new message% [parent label-panel]
                          [label (format "cpu1 bet: ~a " cpu1-bet)]
                          (auto-resize #t)))

(define cpu2-bet-label (new message% [parent label-panel]
                          [label (format "cpu2 bet: ~a " cpu2-bet)]
                           (auto-resize #t)))

(define cpu3-bet-label (new message% [parent label-panel]
                          [label (format "cpu3 bet: ~a " cpu3-bet)]
                           (auto-resize #t)))


;********************************TURNS**************************************

(define start-turn (lambda ()
                     (cond
                       ;setup new round
                       [(= turn 0) (begin (reset-round) (set-turn 1)            
                                   (cond ((not player-in-game) (cpus-raise-check-or-fold))))]
                       ;no cards flipped over bet
                       [(= turn 1) (begin (first-turn) (set-turn 2)
                                   (cond ((not player-in-game) (cpus-raise-check-or-fold))))]
                       ;flip 3 cards in middle
                       [(= turn 2) (begin (second-turn) (set-turn 3)
                                   (cond ((not player-in-game) (cpus-raise-check-or-fold))))]
                       ;flip another card
                       [(= turn 3) (begin (third-turn) (set-turn 4))
                                   (cond ((not player-in-game) (cpus-raise-check-or-fold)))]
                       ;clear the table
                       [(= turn 4) (begin (give-money-to-winner) (reset-player-statuses) (clear-table) (set-turn 0) (start-turn))])))

;new round, draws all the cards
(define reset-round (lambda ()
                      (begin 
                        (cond ((= 0 (send player-money get-balance)) (begin 
                                                                      (send game-over show #t)
                                                                      (exit #t))))
                        (set! first-bet #t)
                        (reset-player-statuses)
                        (set-up-game)
                        ;add all the new cards to the window
                        (add-all-cards-to-window)
                        (reset-bets)
                        (update-money-ui-new-round))))
;flip 3 cards  
(define first-turn 
  (lambda ()
    (begin (send table card-face-up (first mid-cards))
           (send table card-face-up (second mid-cards))
           (send table card-face-up (third mid-cards)))))

;flip 4th card
(define second-turn 
  (lambda ()
    (begin
      (send table card-face-up (fourth mid-cards)))))

;flip last card
(define third-turn 
  (lambda ()
    (begin
      (send table card-face-up (fifth mid-cards)))))


;**********************RESET********************************
;clear the table
(define clear-table
  (lambda ()
    (begin (send table remove-cards your-hand)
           (send table remove-cards cpu1-hand)
           (send table remove-cards cpu2-hand)
           (send table remove-cards cpu3-hand)
           (send table remove-cards mid-cards)
           (send bet-panel delete-child slider)
           (set! slider (new slider%
                    [label "How much would you like to bet?"]
                    [parent bet-panel]
                    [min-value 0]
                    [max-value (send player-money get-balance)]
                    [init-value 0]
                    [style '(vertical-label horizontal)]))
           (send slider show #t))))



;****************** CHECK AND RAISE *************************

(define first-bet #t) ;cpus bet once then this is turned to #f and they only be 


;needs if statement to check if it has been 3 turns since last raise and if player folded TODO
(define cpus-raise-check-or-fold (lambda () 
                                   (begin
                                     (cond ((and cpu1-in-game (or (< cpu1-bet current-bet) first-bet)) (cpu-decide cpu1-hand))) ;if cpu bet less than current bet and in game then go
                                     (cond ((and cpu2-in-game (or (< cpu2-bet current-bet) first-bet)) (cpu-decide cpu2-hand)))
                                     ;if cpu 3 ends up having a turn, and player is currently folded, we will call this function again so that cpu 1 and 2 can go again if cpu3 raised
                                     (cond ((and cpu3-in-game (or (< cpu3-bet current-bet) first-bet)) (begin (cpu-decide cpu3-hand)
                                                                                                              (set! first-bet #f))))
                                     ;(cond ((not player-in-game) (cpus-raise-check-or-fold)))
                                     (update-ui-folds)
                                     (update-ui-bets)
                                     (cond ((or (not player-in-game) (= player-bet current-bet) ;if a cpu raised player needs to check,raise, or fold
                                                    (= player-bet (send player-money get-balance))) (start-turn)) ))))

;so don't start a new turn


                             
(define check-hand (lambda (h) 
                   (begin (check 0) (cpus-raise-check-or-fold) (update-ui-bets))))
                              
(define raise-hand (lambda (h) 
                   (if (> (send slider get-value) current-bet)
                       (begin 
                         (raise 0 (send slider get-value))
                         (cond ((eq? your-hand h) (cpus-raise-check-or-fold)))
                         (update-ui-bets))                  
                   (begin 
                          (send warning-label set-label (format "Current bet is ~a.  If you want to raise, you must bet more than that. " current-bet))
                          (send warning show #t)))))


(define update-money-ui-new-round (lambda () 
                             (begin (update-ui-bets)
                                    (update-ui-new-balances))))


(define update-ui-bets (lambda ()
                         (begin (send your-bet-label set-label (format "Your bet: ~a " player-bet))
                                (send cpu1-bet-label set-label (format "cpu1 bet: ~a " cpu1-bet))
                                (send cpu2-bet-label set-label (format "cpu2 bet: ~a " cpu2-bet))
                                (send cpu3-bet-label set-label (format "cpu3 bet: ~a " cpu3-bet))
                                (send current-bet-label set-label (format "Current bet: ~a " current-bet)))))

(define update-ui-new-balances (lambda ()
                          (begin (send your-balance set-label (format "Your balance: ~a " (send player-money get-balance)))
                                 (send cpu1-balance set-label (format "Cpu1 balance: ~a " (send cpu1-money get-balance)))
                                 (send cpu2-balance set-label (format "Cpu2 balance: ~a " (send cpu2-money get-balance)))
                                 (send cpu3-balance set-label (format "Cpu3 balance: ~a " (send cpu3-money get-balance))))))

                              
;******************FOLDING**********************************

(define fold-hand (lambda (h)
                    (dim-hand h)))

(define dim-hand (lambda (h) 
                   (begin(send (first h) dim #t)
                         (send (second h) dim #t))))

(define update-ui-folds (lambda ()
                          (begin (cond ((and (not(null? your-hand)) (not player-in-game)) 
                                        (begin (send (first your-hand) dim #t)
                                               (send (second your-hand) dim #t))))
                                 (cond ((and (not(null? cpu1-hand)) (not cpu1-in-game)) 
                                        (begin (send (first cpu1-hand) dim #t)
                                               (send (second cpu1-hand) dim #t))))
                                 
                                 (cond ((and (not(null? cpu2-hand)) (not cpu2-in-game)) 
                                        (begin (send (first cpu2-hand) dim #t)
                                               (send (second cpu2-hand) dim #t))))
                                 
                                 (cond ((and (not(null? cpu3-hand)) (not cpu3-in-game)) 
                                        (begin(send (first cpu3-hand) dim #t)
                                              (send (second cpu3-hand) dim #t)))))))

;********************************** Give money to winner **********************************
(define give-money-to-winner (lambda ()
                               (begin
                                 (send table cards-face-up cpu1-hand)
                                 (send table cards-face-up cpu2-hand)
                                 (send table cards-face-up cpu3-hand)
                                 (send table cards-face-up your-hand)
                                 (cond 
                                   ;if player wins
                                   [(= (who-wins)  1) (begin 
                                                         (send player-money set-balance 
                                                               (+ (send player-money get-balance) cpu1-bet cpu2-bet cpu3-bet ))
                                                         (send cpu1-money set-balance (- (send cpu1-money get-balance) cpu1-bet))
                                                         (send cpu2-money set-balance (- (send cpu2-money get-balance) cpu2-bet))
                                                         (send cpu3-money set-balance (- (send cpu3-money get-balance) cpu3-bet))
                                                         (send info-label set-label "You won"))]   
                                   ;if cpu1 wins
                                   [(= (who-wins)  2) (begin 
                                                         (send cpu1-money set-balance
                                                               (+ (send cpu1-money get-balance) player-bet cpu2-bet cpu3-bet ))
                                                         (send player-money set-balance (- (send player-money get-balance) player-bet))
                                                         (send cpu2-money set-balance (- (send cpu2-money get-balance) cpu2-bet))
                                                         (send cpu3-money set-balance (- (send cpu3-money get-balance) cpu3-bet))
                                                         (send info-label set-label "Cpu 1 Won"))]
                                   ;if cpu2 wins
                                   [(= (who-wins)  3) (begin 
                                                         (send cpu2-money set-balance
                                                               (+ (send cpu2-money get-balance) player-bet cpu1-bet cpu3-bet ))
                                                         (send player-money set-balance (- (send player-money get-balance) player-bet))
                                                         (send cpu1-money set-balance (- (send cpu1-money get-balance) cpu1-bet))
                                                         (send cpu3-money set-balance (- (send cpu3-money get-balance) cpu3-bet))
                                                         (send info-label set-label "Cpu 2 Won"))]
                                   ;if cpu 3 wins
                                   [(= (who-wins)  4) (begin 
                                                         (send cpu3-money set-balance
                                                               (+ (send cpu3-money get-balance) player-bet cpu1-bet cpu2-bet ))
                                                         (send player-money set-balance (- (send player-money get-balance) player-bet))
                                                         (send cpu1-money set-balance (- (send cpu1-money get-balance) cpu1-bet))
                                                         (send cpu2-money set-balance (- (send cpu2-money get-balance) cpu2-bet))
                                                         (send info-label set-label "Cpu 3 Won"))]
                                   
                                   [(= (who-wins)  0) (begin
                                                         (send info-label set-label "Tie, everyone keeps their money"))])
                                 (cond((void? (what-did-winner-have?)) (send win-label set-label  ""))
                                      ((= 1 (what-did-winner-have?)) (send win-label set-label  "With Pair"))
                                      ((= 2 (what-did-winner-have?)) (send win-label set-label  "With Two Pairs"))
                                       ((= 3 (what-did-winner-have?)) (send win-label set-label  "With Three-of-a-Kind"))
                                       ((= 4 (what-did-winner-have?)) (send win-label set-label  "With Straight"))
                                       ((= 5 (what-did-winner-have?)) (send win-label set-label  "With Flush"))
                                       ((= 6 (what-did-winner-have?)) (send win-label set-label  "With Straight-Flush"))
                                       ((= 7 (what-did-winner-have?)) (send win-label set-label  "With Royal Flush"))
                                       ((= 0 (what-did-winner-have?)) (send win-label set-label  "")))
                                 (send info show #t))))


;**************BUTTONS****************************************


(define button-panel (new horizontal-panel%	 
     [parent table]
     [alignment '(center center)]))


(define check-button (new button% [parent button-panel]
                          [label "Check (Match bet)"]
                          ; Callback procedure for a button click:
                          [callback (lambda (button event)
                                      (check-hand your-hand))]))

(define raise-button (new button% [parent button-panel]
                          [label "Raise"]
                          ; Callback procedure for a button click:
                          [callback (lambda (button event) ;check if raise is above current bet, if not popup
                                      (raise-hand your-hand))]))

(define fold-button (new button% [parent button-panel]
                         [label "Fold"]
                         ; Callback procedure for a button click:
                         [callback (lambda (button event)
                                     (begin (fold-hand your-hand) ;dim hand
                                            (fold-player)         ;set player-in-game to false
                                            (cpus-raise-check-or-fold)))])) ;cpus take turn to bet,fold, or raise against each other
  
(define cheat-button (new button% [parent button-panel]
                          [label "Cheat"]
                          ; Callback procedure for a button click:
                          [callback (lambda (button event) 
                                      (begin (send table cards-face-up cpu1-hand)
                                             (send table cards-face-up cpu2-hand)
                                             (send table cards-face-up cpu3-hand)
                                             (send table cards-face-up your-hand)))]))


;**************************** MONEY LABELS ***********************

(define money-panel (new horizontal-panel%	 
     [parent table]
     [alignment '(center center)])) 

(define your-balance (new message%	 
  [label (format "Your money: ~a " (send player-money get-balance))]	 
  [parent money-panel]
  [auto-resize #t]))

(define your-bet-label (new message%	 
  [label (format "Your bet: ~a " player-bet)]	 
  [parent money-panel]
  (auto-resize #t)))

(define bet-panel (new horizontal-panel%	 
     [parent table]
     [alignment '(center center)]))

(define slider (new slider%
                    [label "How much would you like to bet?"]
                    [parent bet-panel]
                    [min-value current-bet]
                    [max-value (send player-money get-balance)]
                    [init-value 0]
                    [style '(vertical-label horizontal)]))

                   
;****************SHOW WINDOW******************************
(send table show #t)

;start game
(start-turn)


