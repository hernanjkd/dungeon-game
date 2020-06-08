;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           W E A P O N S    A D T                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTRUCTOR
(define weapon
  (lambda (name description damage)
    (list 'weapon name description damage)))

;SELECTORS
(define weapon?
  (lambda (val)
    (eq? (car val) 'weapon)))

(define weapon-name
  (lambda (wpn)
    (cadr wpn)))

(define weapon-description
  (lambda (wpn)
    (caddr wpn)))

(define weapon-damage
  (lambda (wpn)
    (cadddr wpn)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          O B J E C T    A D T                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

