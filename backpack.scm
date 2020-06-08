(load "object.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          B A C K P A C K    A D T                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONTRUCTOR
(define backpack
  (lambda (capacity)
    (list 'backpack capacity)))

;SELECTORS
(define backpack?
  (lambda (val)
    (eq? 'backpack (car val))))

(define backpack-capacity
  (lambda (bp)
    (cadr bp)))

(define find-object-in-backpack
  (lambda (obj-name bp)
    (cond
      ((eq? (length bp) 2) #f)
      ((eq? obj-name (cadar (cddr bp))) (caddr bp))
      (else
       (find-object-in-backpack obj-name (cdr bp))))))

;MUTATORS
(define add-to-backpack!
  (lambda (obj bp)
    (cond
      ((object? obj)
       (if (>= (cadr bp) (caddr obj))
           (and (set-cdr! (cdr bp) (cons obj (cddr bp)))
                (set-car! (cdr bp) (- (cadr bp) (caddr obj))) #t) #f))
      (else
       #f))))

(define remove-from-backpack!
  (lambda (obj-name bp)
    (letrec ((obj-capacity
              (lambda (nm lst)
                (cond
                  ((eq? (length lst) 2) 0)
                  ((eq? nm (cadar (cddr lst))) (caddar (cddr lst)))
                  (else
                   (obj-capacity nm (cdr lst))))))) 
           (set-car! (cdr bp) (+ (backpack-capacity bp) (obj-capacity obj-name bp))))
    (letrec ((remove
              (lambda (nm lst)
                (cond
                  ((eq? (length lst) 2) '())
                  ((eq? nm (cadar (cddr lst))) (cdddr lst))
                  (else
                   (cons (caddr lst) (remove nm (cdr lst))))))))
      (set-cdr! (cdr bp) (remove obj-name bp)))))

;OTHER OPERATIONS
(define backpack-free-space
  (lambda (bp)
    (backpack-capacity bp)))

(define describe-backpack
  (lambda (bp)    
    (let ((display-more           
           (lambda args
             (for-each display args))))
      (if (= (length bp) 2) (and (display "The backpack is empty.") (newline))
          (and (display "Your backpack contains the following items: ")
               (letrec ((info
                         (lambda (lst)
                           (cond
                             ((null? lst))
                             ((= (length lst) 2) (newline))
                             ((= (length lst) 3) 
                              (and (display-more (cadr (caddr lst)) ".") 
                                   (info (cdr lst))))
                             ((= (length lst) 4)
                              (and (display-more (cadr (caddr lst)) " and ")
                                   (info (cdr lst))))
                            (else
                             (and (display-more (cadr (caddr lst)) ", ") 
                                  (info (cdr lst))))))))
                 (info bp))))
      (display-more "Free slots: " (backpack-free-space bp)))))
    


    

;(define pen
;  (object 'pen 1 "BIC"))
;(define book
;  (object 'book 4 "The Lord of the Rings"))
;(define bookbag
;  (backpack 20))
;(define laptop
;  (object 'laptop 5 "Mac"))
;(define bombacha
;  (object 'bombacha 2 "Medium bombacha que usa Lu"))
;(add-to-backpack! pen bookbag)
;(add-to-backpack! book bookbag)
;(add-to-backpack! bombacha bookbag)
;(add-to-backpack! laptop bookbag)
;bookbag
;(remove-from-backpack! 'bombacha bookbag)
;(remove-from-backpack! 'pen bookbag)
;(remove-from-backpack! 'book bookbag)
;(remove-from-backpack! 'laptop bookbag)
;bookbag
;(add-to-backpack! laptop bookbag)
;bookbag
;(backpack-free-space bookbag)
;(describe-backpack bookbag)
;(find-object-in-backpack 'book bookbag)