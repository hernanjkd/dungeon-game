;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              TRAPS ADT                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Constructor

;Contents:
;1. The name of the trap
;2. The room the trap is in
;3. The amount of damage the trap does
;4. A boolean value that when true the trap is armed and if false if disarmed.
(define trap
  (lambda (trap-name dmg desc disarm-msg)
    (list trap-name dmg #t desc disarm-msg)))

;Selectors

;returns the name of the trap
(define get-trap-name
  (lambda (trap)
    (car trap)))

;returns the damage of the trap
(define get-trap-dmg
  (lambda (trap)
    (cadr trap)))

;returns true if the trap is armed
(define armed?
  (lambda (trap)
    (caddr trap)))

;returns the description of the test
(define get-trap-desc
  (lambda (trap)
    (cadddr trap)))

;Returns the disarm message
(define disarm-msg
  (lambda (trap)
    (cadddr (cdr trap))))


;Mutators

;disarms the trap makeing it harmless to the player
(define disarm-trap!
  (lambda (trap)
    (display (disarm-msg trap))
    (set-car! (cddr trap) #f)))

;OTHER OPERATIONS
;hurt-player: Hurts the player if s/he is still in a trapped room

(define hurt-player
  (lambda (trap plyr)
      (display (get-trap-desc trap))
      (set-car! (cdddr (cdr plyr)) (- (player-hp plyr) (get-trap-dmg trap)))))
