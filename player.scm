;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  COS 125 SEMESTER PROJECT (INTERACTIVE FICTION)                            ;;
;;                                                                            ;;
;;  File: player.scm                                                          ;;
;;                                                                            ;;
;;  Implementation of the basic player ADT.                                   ;;
;;                                                                            ;;
;;  Author:        Erik Albert                                                ;;
;;  Date:          12/20/2007                                                 ;;
;;  Last Modified:                                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   R E P R E S E N T A T I O N                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;;  The representation of the player ADT is a list with four elements.  The
;;  first element is the symbol player, the second is a backpack, the third
;;  is a room, and the last is a boolean value representing whether or not the 
;;  player moved during the last turn.
;;
;;  (player <backpack> <room> <moved?>)   
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          H E L P E R   F U N C T I O N S  (NOT IN INTERFACE)               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     C O N S T R U C T O R S                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: player
;;
;;   Creates and returns a new player with a backpack of size 20 and the 
;;   specified starting location.
;;
;; Input:        start    - An instance of the room ADT.
;; Output:       a new player
;; Side-effects: none

(define player
  (lambda (start)
    ;; Make sure that start is a room.
    (if (not (room? start)) (error 'player "argument is not a room: " start))
    
    ;; Return the new player.  
    (list 'player (backpack 20) start #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        S E L E C T O R S                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: player?
;;
;;   The player type predicate.
;;
;; Input:        val     - Any value
;; Output:       #t/#f
;; Side-effects: none

(define player?
  (lambda (val)
    (and (list? val)
         (= (length val) 4)
         (eq? 'player (car val)))))


;.
;; Function: player-location
;;
;;   Returns the player's current location (a room).
;;
;; Input:        plyr     - a player
;; Output:       the location of plyr (a room)
;; Side-effects: none

(define player-location
  (lambda (plyr)
    
    ;; Make sure that plyr is a player
    (if (not (player? plyr)) (error 'player-location "argument is not a player: " plyr))
    
    ;; Return the location
    (list-ref plyr 2)))

;.
;; Function: player-backpack
;;
;;   Returns the player's backpack.
;;
;; Input:        plyr     - a player
;; Output:       a backpack
;; Side-effects: none

(define player-backpack
  (lambda (plyr)
    
    ;; Make sure that plyr is a player
    (if (not (player? plyr)) (error 'player-location "argument is not a player: " plyr))
    
    ;; Return the backpack
    (list-ref plyr 1)))


;.
;; Function: player-moved?
;;
;;   Returns true if the player moved during the last turn.
;;
;; Input:        plyr     - a player
;; Output:       true/false
;; Side-effects: none

(define player-moved?
  (lambda (plyr)
    
    ;; Make sure that plyr is a player
    (if (not (player? plyr)) (error 'player-location "argument is not a player: " plyr))
    
    ;; Return the moved? flag
    (list-ref plyr 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         M U T A T O R S                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: set-player-location!
;;
;;   Changes the location of the player.
;;
;; Input:        plyr     - a player
;;               rm       - a new room
;; Output:       nothing
;; Side-effects: plyr is modified

(define set-player-location!
  (lambda (plyr rm)
    
    ;; Make sure that plyr is a player
    (if (not (player? plyr)) (error 'player-location "first argument is not a player: " plyr))
    
    ;; Make sure that rm is a room
    (if (not (room? rm)) (error 'player-location "second argument is not a room: " rm))

    ;; Change the location
    (set-car! (list-tail plyr 2) rm)))


;.
;; Function: set-player-moved!
;;
;;   Changes the flag that represents whether or not the player moved during
;;   the last turn.
;;
;; Input:        plyr     - a player
;;               moved?   - true/false
;; Output:       nothing
;; Side-effects: plyr is modified

(define set-player-moved!
  (lambda (plyr moved?)
    
    ;; Make sure that plyr is a player
    (if (not (player? plyr)) (error 'player-location "first argument is not a player: " plyr))
    
    ;; Make sure that rm is a room
    (if (not (boolean? moved?)) (error 'player-location "second argument is a boolean value: " moved?))

    ;; Change the location
    (set-car! (list-tail plyr 3) moved?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 O T H E R   O P E R A T I O N S                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: describe-player
;;
;;   Prints useful information about a player to the display.
;;
;; Input:        plyr     - a player
;; Output:       nothing
;; Side-effects: prints to the display

(define describe-player 
  (lambda (plyr)
    (display-nl "You're a fighter. You must train and study hard.")
    (describe-backpack (player-backpack plyr))))




