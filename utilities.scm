;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  COS 125 SEMESTER PROJECT (INTERACTIVE FICTION)                            ;;
;;                                                                            ;;
;;  File: utilities.scm                                                       ;;
;;                                                                            ;;
;;  Useful utility functions for creating interactive fiction in the Scheme   ;;
;;  programming language.                                                     ;;
;;                                                                            ;;
;;  Author:        Erik Albert                                                ;;
;;  Date:          10/23/2006                                                 ;;
;;  Last Modified: 01/05/2007 (added debugging mode procedures                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;.
;; Procedure: set-debug-mode!
;; 
;;  Turns debugging mode on or off (initially off).
;;
;; Input: state (should be either #t or #f)
;; Output: none
;; Side-effects: changes debugging mode

(define set-debug-mode! #f)  ; shares state with debug-msg, see below

;. 
;; Procedure: debug-msg
;;
;;  Prints a debugging message (if in debug mode) or does nothing (if not
;;  in debug mode).
;;
;; Input: Any number of objects to be printed (like display-more-nl)
;; Output: none
;; Side-effects: if in debugging mode, the message will be printed to the screen

(define debug-msg  #f)       ; shares state with set-debug-mode!, see below


;; Actual implementation of set-debug-mode! and debug-msg.  These
;; procedures share a state variable.
(let ((debug #f)) ; If not #f, we're in debug mode and messages will be printed
  
  ;; Create set-debug-mode!
  (set! set-debug-mode! 
        (lambda (state) 
          (set! debug state)))

  ;; Create debug-msg
  (set! debug-msg 
        (lambda msg 
          (cond
            (debug 
             (for-each (lambda (obj)
                         ;; Use debug for strings, write for everything else
                         (if (string? obj) (display obj) (write obj)))
                       (cons "DEBUG: " msg))
             ;; print a newline
             (newline)))))
  );let


;. 
;; Procedure: error
;;
;;   Signals an error condition and causes your program to stop.
;;
;; Input:  pname  -  The name of the procedure (a symbol)
;;         mess ...  -  Objects printed as the error message.  Printed using
;;                      display-more-nl (see below).
;; Output: none
;; Side-Effect: program dies

(define error
  (lambda (pname . mess)
    (apply display-more-nl (cons "Error in " (cons pname (cons ": " mess))))
    ('error-condition) ; This will cause a real error
    ))


 
;.
;; Procedure: setup-random
;;
;;   Call this procedure with a random number seed.  A seed is a number that
;;   picks a sequence of numbers.  If you choose the same number twice, the 
;;   sequence of random numbers made will be the same.  
;;
;;   You only need to call this once per time your program is run, and only if
;;   you can give it a different number each time.  Maybe you should have the 
;;   player pick a number between 1 and 10,000. 
;;
;;   setup-random and roll share the same state (see the assignment handout).
;;
;; Input:  seed  -  an integer
;; Output: 
  
(define random-setup #f)    ; Defined Below

;. 
;; Procedure: roll
;;
;;   Rolls a die to get a random number.  The argument sides picks the size
;;   of the die to roll.  
;;   
;;   The number picked will be between 1 and sides (inclusive).
;;
;; Input:  sides  -  The number of sides of the die (any positive integer)
;; Output: A random number from 1, 2, 3 ... sides

(define roll #f)            ; Defined Below

;; random-setup and roll
;; Share internal state rng with is a random number generator made 
;; with random-maker (see below).
(let ((rng #f))

  (set! roll
        (lambda (sides)
          (if (not rng) (set! rng (random-maker 9179872361)))
          (+ 1 (rng sides))))
  
  (set! random-setup
        (lambda (seed)
          (set! rng (random-maker seed)))))


;.
;; Procedure: but-last
;;
;;   Returns a list with the same elements as the input list, with the
;;   last element removed.
;;
;; Input:  lst  -  a list
;; Output: a list with the same elements as lst, but the last element removed.

(define but-last
  (lambda (lst)
    (cond ((null? lst) lst)
          ((null? (cdr lst)) (cdr lst))
          (else (cons (car lst) (but-last (cdr lst)))))))


;.
;; Procedure: last
;; 
;;  Given a list, returns the last element in the list.
;;
;; Input:  lst  -  a list
;; Output: The last element of lst, or #f if lst is empty.

(define last
  (lambda (lst)
    (cond ((null? lst) #f)
          ((null? (cdr lst)) (car lst))
          (else (last (cdr lst))))))
        

;.
;; Procedure: add-to-end
;; 
;; Recursive procedure from in-class example.  Given an item and a list, 
;; recursively creates a new list with the same items in the original list,
;; but with the new item at the end.
;;
;; Input: object  -  any value
;;        list    -  a list
;; Output: A list with the same items as (append list (list object))

(define add-to-end
  (lambda (object list)
    (cond 
      ((null? list) (cons object '())) ; Empty list termination condition
      (else 
       (cons (car list) (add-to-end object (cdr list)))))))


;.
;; Procedure: display-nl
;; 
;; Same thing as display, but prints a newline after.
;;
;; Input: arg - any value
;; Output: none
;; Side-effects: prints to the display

(define display-nl
  (lambda (arg)
    (display arg)
    (newline)))


;.
;; Procedure: display-more
;; 
;; Takes any number of arguments and is equivalent of calling
;; display on each argument individually.
;;
;; Input: args - any values
;; Output: none
;; Side-effects: prints to the display

(define display-more
  (lambda args
    (for-each display args)))


;.
;; Procedure: display-more-nl
;; 
;; Same thing as display, but prints a newline after.
;;
;; Input: args - any values
;; Output: none
;; Side-effects: prints to the display

(define display-more-nl
  (lambda args
    (apply display-more args)
    ;; We probably won't cover apply, but look it up in the Scheme book.
    (newline)))
    

;.
;; Procedure: display-english-list
;; 
;; Display's a list using English list syntax.  
;;
;;  Example: (display-english-list '(A B C)) prints:
;;
;;  A, B, and C
;;
;; Input: args - any values
;; Output: none
;; Side-effects: prints to the display

(define display-english-list
  (lambda (lst)
    (letrec ((finish-list ; what in the world is a letrec?
              (lambda (lst)
                (cond 
                  ((null? lst)) ; do nothing
                  ((null? (cdr lst)) 
                   (display-more ", and " (car lst)))
                  (else 
                   (display-more ", " (car lst))
                   (finish-list (cdr lst)))))))
      (cond
        ((null? lst) 
         (display-nl "nothing"))
        ((= 2 (length lst))
         (display-more-nl (car lst) " and " (cadr lst)))
        (else (display (car lst))
              (finish-list (cdr lst))
              (newline))))))


;.
;; Procedure: read-number
;; 
;; Requires the player to enter a number, and returns the number.  The number
;; is finished when the player presses the enter key.  If the player does not 
;; enter a number, read-number will display a message and will start over.
;;
;; Input: none
;; Output: a number

(define read-number
  (lambda ()
   
    ;; This is a general purpose do loop.  Each iteration, the variable 
    ;; num will be set to (string->number (read-string)).
    (do ((num (string->number (read-string)) (string->number (read-string))))
        ;; we're not going to talk about do in this class, but you can read
        ;; about it in your textbook.  Basically, this loop will stop running
        ;; when the variable num is a number and return it.
        ((number? num) num) ; this is the loop termination test
      
      ;; A polite message to try again -- please feel free to customize it.
      (display "A number is required, please try again:")
      (newline))))
      
      
;.
;; Procedure: read-string
;; 
;; Requires the player to enter a string, and returns the string.  The string
;; is finished when the player presses the enter key.
;;
;; Input: none
;; Output: a string

(define read-string
  (lambda ()
    
    ;.
    ;; Procedure: get-chars
    ;; 
    ;; Returns a list of characters entered by the user.  This recursive
    ;; procedure terminates when the user presses the enter key.
    ;;
    ;; Only used as a helper function by read-string.
    ;;
    ;; Input: none
    ;; Output: a list of the characters entered by the player

    (define get-chars
      (lambda ()
        (let ((c (read-char)))          ; read the next char entered
          (if (or (eq? c #\newline) (eq? c #\newline)) '()      ; linefeed/newline is termination condition
              (cons c (get-chars))))))  ; add the character to the list
    
    ;; convert the list of characters to a string and return it
    (list->string (get-chars))))


;.
;; Procedure: read-command
;; 
;; Requires the player to enter a command.  A command is a sequence of 
;; symbols and numbers, and the entire command will be returned as a list.
;;
;; Input: none
;; Output: a command list

(define read-command
  (lambda ()
    
    (let ((raw-command (add-to-end #\newline (string->list (read-string)))) 
          (current-symbol '())            ; List that will be next symbol
          (command-list '()))             ; List that will be returned
      
      ;; Read a string from the user
      ;; Turn the string into a list with string->list
      ;; Add a newline to the end of the list (so we save last symbol)
      ;; Create symbols/words from items in the list (seperated by spaces)
      ;; Ignore extra spaces
      (for-each 
       (lambda (c)
         (cond ((or (eq? #\space c) (eq? #\newline c))  ; Delimeters
                (if (not (null? current-symbol))        ; Finished a symbol?
                    (let ((str (list->string current-symbol)))
                      ;; We will use str to make either a symbol or number
                      (set! command-list  ; Add new value to end of command list
                            (add-to-end 
                             (if (string->number str)  ; Is it a number?
                                 (string->number str)  ; Make a number
                                 (string->symbol str)) ; Make a symbol
                             command-list))
                      (set! current-symbol '()))))
               (else
                ;; Put the character in the list for the current symbol/number
                (set! current-symbol 
                      (add-to-end (char-downcase c) current-symbol))
                )
               )) raw-command)
      command-list)))

;.
;; Procedure: random-maker
;;
;;  Creates procedures that are random number generators.
;;  I didn't write this, it's from:
;;
;;  http://www.math.grin.edu/~stone/events/scheme-workshop/random.html
;;
;;  You don't need to ever use this!

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647)
         (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus
                  candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
                19860617))))  ;; an arbitrarily chosen birthday
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed))
               (/ (- modulus seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1
                           exact-top)))
                 (set! seed (apply-congruence seed))
                 (inexact->exact (floor (* top (/ seed modulus))))))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))