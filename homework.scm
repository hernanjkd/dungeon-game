;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  COS 125 SEMESTER PROJECT (INTERACTIVE FICTION)                            ;;
;;                                                                            ;;
;;  File: homework.scm                                                        ;;
;;                                                                            ;;
;;  Implementation of the basic homework ADT.                                 ;;
;;                                                                            ;;
;;  Author: Semester Project Group 4A (Hernan Garcia)                         ;;
;;  Date: 04/23/08                                                            ;;
;;  Last Modified: 04/23/08                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   R E P R E S E N T A T I O N                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The homework data type is a list with the following values:
; 0. Name of the homework
; 1. Grade of the homework. The grading will depend on the amount of red-bulls 
;    that the player has in his backpack. 
; 2. Returns true if the homework has been stapled or #f if not.
; 3. Returns true if the homework has been completed or #f if not.
; 4. Returns true if the player opted to take the homework

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        H E L P E R    F U N C T I O N S                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;count
;Counts how many of the same objects are in the given list.

(define count
  (lambda (obj lst)
    (cond
      ((null? lst) 0)
      ((eq? (car lst) obj)(+ 1 (count obj (cdr lst))))
      (else
       (count obj (cdr lst))))))

;count-same-object-in-backpack:
;Returns how many of the same object is in the backpack.

(define count-same-object-in-backpack
  (lambda (objname bp)
    (count objname (caddr bp))))


;Objlist-find-object:
;Finds the given object in the list, otherwise returns #f.

(define objlist-find-object
  (lambda (obj-name lst)
    (cond
      ((null? lst) #f)
      ((eq? obj-name (object-name (car lst))) (car lst))
     (else
      (objlist-find-object obj-name (cdr lst))))))

;find-object-in-backpack:
;If obj-name is the name of an object in the backpack (bp), the named object 
;will be returned, otherwise #f is returned.

(define find-object-in-backpack
  (lambda (obj-name bp)
    (objlist-find-object obj-name (caddr bp))))



;grade
;Returns the grade depending on the number given.

(define grade
 (lambda (n)
  (case n
    ((0) 'F)
    ((1) 'D)
    ((2) 'C)
    ((3) 'B)
    ((4) 'A))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           C O N S T R U C T O R S                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Homework constructor:
;Takes in the name, grade, if it's stapled, if it's completed, and if it has been
;taken.

(define homework
  (lambda ()
    (list 'homework 'F #f #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             S E L E C T O R S                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Homework type predicate
;Returns #t if the variable passed to it is a homework as defined in the above
;constructor.

(define homework?
  (lambda (hmw)
    (and
      (not (null? hmw))
      (list? hmw)
      (symbol? (car hmw))
      (symbol? (cadr hmw))
      (boolean? (caddr hmw))
      (boolean? (cadddr hmw))
      (boolean? (cadddr (cdr hmw))))))

;homework-name
;Returns the name of the homework.

(define homework-name
  (lambda (hmw)
    (car hmw)))

;homework-grade
;Returns the grade recieved (A, B, C, D or F.)

(define homework-grade
  (lambda (hmw)
    (cadr hmw)))

;homework-stapled
;Returns #t if stapled or #f if not.

(define homework-stapled?
  (lambda (hmw)
    (caddr hmw)))

;homework-completed
;Returns #t if completed or #f if not.

(define homework-completed?
  (lambda (hmw)
    (cadddr hmw)))

;homewor-taken?
;Returns #t if the homework has been taken, otherwise returns #f.

(define homework-taken?
  (lambda (hmw)
    (cadddr (cdr hmw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              M U T A T O R S                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;grade-homework!
;Grades the homework depending on the amount of red-bulls that are in the backpack.

(define grade-homework!
  (lambda (hmw plyr)
    (cond
     ((not (homework? (player-hmw plyr))) (display-nl "James looks at you weird and says, 'Uhm, you don't have your homework.  Take a seat."))
     ((not (homework-stapled? hmw)) (set-car! (cdr hmw) 'F))
     ((not (homework-completed? hmw)) (set-car! (cdr hmw) 'F))
     ((not (homework-taken? hmw)) (set-car! (cdr hmw) 'F))
     (else
      (set-car! (cdr hmw) (grade (count-same-object-in-backpack (find-object-in-backpack 'red-bull (player-backpack plyr)) (player-backpack plyr))))))))
      
;staple-homework!
;Staples the homework.

(define staple-homework!
  (lambda (hmw)
    (if (homework? hmw)
        (set-car! (cddr hmw) #t))))

;complete-homework!
;Completes the homework.

(define complete-homework!
  (lambda (hmw)
    (if (homework? hmw)
        (set-car! (cdddr hmw) #t))))

;take-homework!
;Procedure that allows the player to take the homework.

(define take-homework!
  (lambda (hmw)
       (set-car! (cddddr hmw) #t)))


;add-hmw-to-player!
;adds the homework to the player
(define add-hmw-to-player!
  (lambda (hmw plyr)
     (set-car! (cddddr (cdr plyr)) (cons hmw (cadddr (cddr plyr))))))
