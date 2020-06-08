(load "object.scm")
(load "backpack.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              R O O M    A D T                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;After 'room name description near-desc... the empty list is for the objects, the last list is to store the rooms connected: North, South, West, East.

;CONSTRUCTOR
(define room
  (lambda (name description near-desc)
    (list 'room name description near-desc () (list #f #f #f #f))))

;SELECTORS
(define room?
  (lambda (val)
    (eq? (car val) 'room)))

(define room-name
  (lambda (rm)
    (cadr rm)))

(define room-description
  (lambda (rm)
    (caddr rm)))

(define room-near-desc
  (lambda (rm)
    (cadddr rm)))

(define find-object-in-room
  (lambda (obj-name rm)
    (letrec ((find
              (lambda (obj lst)
                (cond
                  ((eq? (length lst) 0) #f)
                  ((eq? (cadar lst) obj) (car lst))
                  (else
                   (find obj (cdr lst)))))))
      (find obj-name (car (cddddr rm))))))

(define objects-in-room
  (lambda (rm)
    (let ((display-more
           (lambda args
             (for-each display args))))
      (letrec ((names
                (lambda (r)
                  (cond
                    ((null? r) (display "none."))
                    ((= (length r) 1) (display-more (cadar r) "."))
                    ((= (length r) 2) (display-more (cadar r) " and " (cadadr r) "."))
                    (else
                     (and (display-more (cadar r) ", ") (names (cdr r))))))))
        (names (car (cddddr rm)))))))

(define room-north
  (lambda (rm)
    (caaddr (cdddr rm))))

(define room-south
  (lambda (rm)
    (cadadr (cddddr rm))))

(define room-west
  (lambda (rm)
    (caddar (cddddr (cdr rm)))))

(define room-east
  (lambda (rm)
    (cadddr (cadddr (cddr rm)))))

;MUTATORS
(define add-object-to-room!
  (lambda (obj rm)
    (if (object? obj) (set-car! (cddddr rm) (cons obj (car (cddddr rm)))))))

(define remove-object-from-room!
  (lambda (obj-name rm)
    (letrec ((remove
              (lambda (obj lst)
                (cond
                  ((eq? (length lst) 0) '())
                  ((eq? obj (cadar lst)) (cdr lst))
                  (else
                   (cons (car lst) (remove obj (cdr lst))))))))
      (set-car! (cddddr rm) (remove obj-name (car (cddddr rm)))))))

(define set-room-north!
  (lambda (rm north)
    (set-car! (cadddr (cddr rm)) north)))

(define set-room-south!
  (lambda (rm south)
    (set-car! (cdaddr (cdddr rm)) south)))

(define set-room-west!
  (lambda (rm west)
    (set-car! (cddadr (cddddr rm)) west)))

(define set-room-east!
  (lambda (rm east)
    (set-car! (cdddar (cddddr (cdr rm))) east)))

(define connect-rooms!
  (lambda (dir rmne rmsw)
    (cond
      ((eq? dir 'ew) (and (set-room-west! rmne rmsw) (set-room-east! rmsw rmne)))
      ((eq? dir 'ns) (and (set-room-south! rmne rmsw) (set-room-north! rmsw rmne)))
      (else
       (#f)))))

;OTHER OPERATIONS
(define describe-room
  (lambda (rm)
    (let ((displaymorenl
           (lambda args
             (for-each display args)
             (newline))))
      (displaymorenl (room-description rm))
      (newline)
      (display "Objects present: ") (objects-in-room rm)
      (newline)
      (newline)
      (if (room-north rm) (displaymorenl "North: " (room-near-desc (room-north rm))))
      (if (room-west rm) (displaymorenl "West: " (room-near-desc (room-west rm))))
      (if (room-south rm) (displaymorenl "South: " (room-near-desc (room-south rm))))
      (if (room-east rm) (displaymorenl "East: " (room-near-desc (room-east rm)))))))
  







;(define bg
;  (backpack 8))
;(define kitchen
;  (room 'l 'k 's))
;(define valesrm
;  (room "Martha B. Rabbit" "Very messy because she had a sleepover." "You can hear the flush of a toilet from the bathroom."))
;(define pen
;  (object 'pen 1 "BIC"))
;(define book
;  (object 'book 4 "The Lord of the Rings"))
;(define laptop
;  (object 'laptop 5 "Mac"))
;(define lantern
;  (object 'Lucy 8 "It's white and it can turn to face the roof or the floor"))
;valesrm
;(add-object-to-room! lantern valesrm)
;(add-object-to-room! pen valesrm)
;(add-object-to-room! book valesrm)
;(add-object-to-room! laptop valesrm)
;valesrm
;(remove-object-from-room! 'lucy valesrm)
;(remove-object-from-room! 'pen valesrm)
;(remove-object-from-room! 'book valesrm)
;(remove-object-from-room! 'laptop valesrm)
;(objects-in-room valesrm)
;(newline)
;(set-room-north! valesrm 'norte)
;(set-room-south! valesrm 'sur)
;(set-room-west! valesrm 'oeste)
;(set-room-east! valesrm 'este)
;(room-north valesrm)
;(room-south valesrm)
;(room-west valesrm)
;(room-east valesrm)
;(find-object-in-room 'lucy valesrm)
;valesrm