;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  COS 125 SEMESTER PROJECT (INTERACTIVE FICTION)                            ;;
;;                                                                            ;;
;;  File: game-loop.scm                                                       ;;
;;                                                                            ;;
;;  Implementation of the Interactive Fiction command loop.                   ;;
;;                                                                            ;;
;;  Author:        Erik Albert                                                ;;
;;  Date:          12/21/2007                                                 ;;
;;  Last Modified:                                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;.
;; Function: game-loop
;;
;;   The Interactive Fiction command loop.  Keeps recursing until the player
;;   enters the "quit" command.
;;
;; Input:        plyr   - an instance of the player ADT
;;               turn   - the current turn (an integer)
;; Output:       nothing
;; Side-effects: prints information to the display, reads commands from user

(define game-loop 
  (lambda (plyr turn)
    (let ((command '()))       ; The command that the player enters
    
      ;; If the player moved, describe the current location, then reset
      ;; the flag.
      (cond 
        ((player-moved? plyr) 
         (describe-room (player-location plyr))
         (set-player-moved! plyr #f)))
        
      ;; Prompt the player to enter a command
      (newline)
      (display-nl "What would you like to do now?")
      (set! command (read-command)) ; Get the command from the user.
        
      ;; Print the command list (only in debug mode)
      (debug-msg "Command list: " command)
        
      ;; Check for the termination condition (player enters quit)
      (cond
        ((equal? command '(quit))
         (display-nl "Goodbye."))

        (else
         ;; Figure out what to do based on the command.
         (cond 
          
          ;; Nothing Entered -- reprompt
          ((null? command))
                    
          ;; TEMPLATE: Additional commands should be added as additional CLAUSES of this COND
          
          ;; The STATS Command -- Shows the player's statistics
          ((eq? (car command) 'stats) 
           (describe-player plyr))
          
          ;; The WHEREAMI Command -- Displays the location/room information
          ((eq? (car command) 'whereami) 
           (set-player-moved! plyr #t)) ; trick game into thinking that player moved  
          
          ;; The TAKE Command -- Picks up Objects
          ((eq? (car command) 'take)
           (cond
             ;; Make sure that the player entered an object name
             ((not (= (length command) 2))
              (display-nl "TAKE expects one argument: take object"))
             
             ;; Make sure that the object is in the room
             ((not (find-object-in-room (cadr command) (player-location plyr)))
              (display-more-nl "There's no " (cadr command) " here."))
             
             ;; Try to put the object in your backpack
             (else
              (cond 
                ;; It fits!
                ((add-to-backpack! (find-object-in-room (cadr command) (player-location plyr)) 
                                  (player-backpack plyr))
                 (display-more-nl "You put the " (cadr command) " in your backpack.")
                 (remove-object-from-room! (cadr command) (player-location plyr)))

                ;; It does not fit.
                (else
                 (display-more-nl "You try to put the " (cadr command) " in your backpack, but you can't get it to fit."))))))
          
          
          ;; The DROP command
          ((eq? (car command) 'drop)
           (cond
             ;; Make sure that the player entered an object name
             ((not (= (length command) 2))
              (display-nl "DROP expects one argument: drop object"))
             
             ;; Make sure that the object is in the backpack
             ((not (find-object-in-backpack (cadr command) (player-backpack plyr)))
              (display-more-nl "There's no " (cadr command) " in your backpack."))
             
             ;; Drop the object
             (else
              ;; Add to room
              (add-object-to-room! (find-object-in-backpack (cadr command) (player-backpack plyr))
                                   (player-location plyr))
              (display-more-nl "You carefully place the " (cadr command) " on the floor in " (room-name (player-location plyr)) ".")
              
              ;; remove from backpack
              (remove-from-backpack! (cadr command) (player-backpack plyr)))))
             
          
          ;; The USE command
          ;; This one is tricky because the object can either be in the room or in the backpack
          ((eq? (car command) 'use)
           (cond
             ;; Make sure that the player entered an object name
             ((not (= (length command) 2))
              (display-nl "USE expects one argument: use object"))
             
             (else
              ;; It doesn't matter if the object is in the room or backpack
              (let ((object (or (find-object-in-backpack (cadr command) (player-backpack plyr))
                                (find-object-in-room (cadr command) (player-location plyr)))))
                ;; Make sure that the object's here before using it.
                (if (not object)
                    (display-more-nl "There's no " (cadr command) " to use.")
                    (use-object object plyr))))))
          
          
          ;; The EXAMINE command
         ((eq? (car command) 'examine)
           (cond
             ;; Make sure that the player entered an object name
             ((not (= (length command) 2))
              (display-nl "EXAMINE expects one argument: examine object"))
             
             (else
              ;; It doesn't matter if the object is in the room or backpack
              (let ((object (or (find-object-in-backpack (cadr command) (player-backpack plyr))
                                (find-object-in-room (cadr command) (player-location plyr)))))
                ;; Make sure that the object's here before examining it.
                (if (not object)
                    (display-more-nl "There's no " (cadr command) " here.")
                    (display-nl (object-description object)))))))
                   
          
          ;; The GO Command -- Moves the player
          ((eq? (car command) 'go)
           (cond 
             ((not (= 2 (length command)))
              (display-nl "GO expects one argument: go direction"))
             ((not (member (cadr command) '(north south east west)))
              (display-nl "GO DIRECTION must be one of north, south, east or west"))
             (else
              (cond 
                ;; NORTH
                ((and (eq? (cadr command) 'north) ; Player wants to go North
                      (room-north (player-location plyr))) ; There is a room to the North
                 ;; Move!
                 (set-player-moved! plyr #t)
                 (set-player-location! plyr (room-north (player-location plyr))))
                
                ;; SOUTH
                ((and (eq? (cadr command) 'south) ; Player wants to go South
                      (room-south (player-location plyr))) ; There is a room to the South
                 ;; Move!
                 (set-player-moved! plyr #t)
                 (set-player-location! plyr (room-south (player-location plyr))))

                ;; EAST
                ((and (eq? (cadr command) 'east) ; Player wants to go East
                      (room-east (player-location plyr))) ; There is a room to the East
                 ;; Move!
                 (set-player-moved! plyr #t)
                 (set-player-location! plyr (room-east (player-location plyr))))

                ;; WEST
                ((and (eq? (cadr command) 'west) ; Player wants to go West
                      (room-west (player-location plyr))) ; There is a room to the West
                 ;; Move!
                 (set-player-moved! plyr #t)
                 (set-player-location! plyr (room-west (player-location plyr))))
                
                ;; Bad direction or no room in direction given
                (else
                 (display-more-nl "You can't move to the " (cadr command)))))))
          
          ;; UNKNOWN COMMAND
          (else (display-more-nl "You don't know how to " (car command) "!"))
          ) ; cond (command handling cond)
                 
         ;; Recursive call (continue the game loop)
         (game-loop plyr (+ turn 1))
                 
         ) ; else
        ) ; cond (termination condition cond)
      ); let (game local variables)
    ); lambda
  ); define

