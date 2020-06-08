;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  COS 125 SEMESTER PROJECT (INTERACTIVE FICTION)                            ;;
;;                                                                            ;;
;;  File:        game.scm                                                     ;;
;;  Authors:     Erik Albert                                                  ;;
;;               and                                                          ;;
;;               your names go here                                           ;;
;;                                                                            ;;
;;  Game Title:  your game title goes here                                    ;;
;;                                                                            ;;
;;  This file loads all of the required game files to play the game and       ;;
;;  implements the "play-game" procedure.                                     ;;
;;                                                                            ;;
;;  TEMPLATE :: This is a template file; please update for your game!         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "utilities.scm")
(load "testmap.scm")
(load "game-loop.scm")
(load "player.scm")      ; The player ADT
(load "object.scm")      ; The object ADT
(load "backpack.scm")    ; The backpack ADT
(load "room.scm")        ; The room ADT
(load "dialogtree.scm")  ; The dialog tree ADT

; load additional files here



;; The following procedure call turns debugging mode on, meaning that all of
;; the debug-msg procedure calls will be printed to the screen.  When your 
;; program is finished (i.e., no additional debugging is needed, you can 
;; remove this line and no debugging messages will be printed.
;; 
;; It is recommended that you do not remove the debug-msg statements below 
;; so that if something breaks, you can turn the debugging mode back on.

;;(set-debug-mode! #t)


;.
;; Procedure: play-game
;;
;; This is the main procedure for the game.  This procedure sets up the game
;; environment and starts the main loop of the game.  
;;
;; Input: none
;; Output: none
;; Side-effects: none

(define play-game
  (lambda ()
    (let ((the-player #f))       ; the player variable (set below)

      ;; Print the game starting information.
      ;; Customize for your game.
      (display-nl "--- TURN-IT-IN: The COS 125 Example Game ---")(newline)
      
      ;; Create the map and the player. (create-map) creates the map and returns
      ;; the starting location, and that location is passed to the player ADT's 
      ;; constructor for the LOCATION property. 
     
      (set! the-player (player (create-map)))
      
      ;; TEMPLATE: If you need to tell the user anything at the beginning
      ;; of the story, do so here (before the game loop).
      (newline)
      (display-nl "Your goal is to finish the latest programming assignment and submit it to James before it's too late!")
      (newline)
      (display-nl "(Creative, I know.)")
      (newline)

      ;; The Main Game Loop
      ;; Turn counter is 0
      (game-loop the-player 0))))

(play-game)