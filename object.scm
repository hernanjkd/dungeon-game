;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            O B J E C T    A D T                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTRUCTOR
(define object
  (lambda (name size description)
    (list 'object name size description (lambda (obj plyr)
                                          (display "Check the object constructor!!")
                                          (newline)))))

;SELECTORS
(define object?
  (lambda (val)
    (eq? 'object (car val))))

(define object-name
  (lambda (obj)
    (cadr obj)))

(define object-size
  (lambda (obj)
    (caddr obj)))

(define object-description
  (lambda (obj)
    (cadddr obj)))

(define use-object
  (lambda (obj plyr)
    (car (cddddr obj))))

;MUTATORS
(define set-object-use-action!
  (lambda (obj proc)
    (set-car! (cddddr obj) proc)))