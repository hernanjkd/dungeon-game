;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COS 125 - Fall 2007                                              12/4/2007 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Interactive Fiction Dialog Trees
;;
;; All branch nodes are dialog that a character speaks.
;;
;; All leaf nodes have dialog and return values. The return value specifies 
;; the result of the conversation.
;;
;; Nodes are connected by labeled arcs; the label is the answer 
;; supplied by the user.  
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         HELPER FUNCTIONS -- NOT PART OF DIALOG TREE INTERFACE              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: get-arc-labels
;; 
;;   Takes a list of dialog tree nodes and returns a list of all of the 
;;   arc labels (without looking at any children).
;;
;; Input: A list of dialog tree nodes
;; Output: A list of strings
;; Side-effects: none
;.

(define get-arc-labels    ;; Like names exercise from HW7    
  (lambda (lst-nodes)
    (if (null? lst-nodes) '()
        (cons (arc-label (car lst-nodes))
              (get-arc-labels (cdr lst-nodes))))))


;.
;; Function: find-node
;; 
;;   Takes a list of nodes and an arc label, and returns the node with
;;   the given label (or #f if there is none).
;;
;; Input: A list of dialog trees
;; Output: A dialog tree or #f
;; Side-effects: none
;.

(define find-node
  (lambda (lst-nodes label)
    (if (null? lst-nodes) #f ; not found
        (if (string=? (arc-label (car lst-nodes)) label) ; found
            (car lst-nodes)
            (find-node (cdr lst-nodes) label)))))

;.
;; Function: print-choices
;; 
;;   Prints a numbered set of responses that the player can 
;;   choose from.
;;
;; Input:   num - the number of the next option
;;          lst - a list of responses
;; Output: none
;; Side-effects: prints to the display
;.

(define print-choices 
  (lambda (num lst)
    (cond 
      ((not (null? lst))
       (display-more-nl num ". " (car lst))
       (print-choices (+ num 1) (cdr lst))))))

;.
;; Function: get-valid-choice
;; 
;;   Keeps reprompting until the user chooses a valid response.
;;
;; Input: a list of responses
;; Output: the chosen response (the string, not the number)
;; Side-effects: prints to the screen, requests input
;.

;; NOTE (1): 
;; Kinda different, because our termination condition depends on the
;; user -- the input never changes!

;; NOTE (2):
;; I won't/didn't use inexact->exact and truncate in class to avoid
;; confusion.  You can/should look them up in the Scheme book.  They 
;; are used to make sure that list-ref gets an integer (otherwise 
;; there would be an error if the player entered something like 1.3).

(define get-valid-choice
  (lambda (lst)
    (print-choices 1 lst)
    (let ((choice (- (inexact->exact (truncate (read-number))) 1)))
      (cond
        ((< choice (length lst))
         (list-ref lst choice))
        (else
         (display-nl "Please choose one of the listed options.")
         (get-valid-choice lst))))))
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTRUCTORS                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: dtree-leaf
;; 
;;   Creates a new dialog tree leaf node.
;;
;; Input: label   - the label of the arc that points to this node
;;        message - the character's dialog
;;        retval  - the return value for the conversation
;; Output: a new dialog tree leaf node
;; Side-effects: none
;.

(define dtree-leaf
  (lambda (label message retval)
    (list label message '() retval)))


;.
;; Function: dtree
;; 
;;   Creates a new dialog tree.
;;
;; Input: label    - the label of the arc that points to this node
;;        message  - the character's dialog
;;        child    - the first child tree (required)
;;        children - all of the other children (this variable is a list!)
;; Output: a new dialog tree
;; Side-effects: none
;.

(define dtree
  (lambda (label message child . children)
    (list label message (cons child children) 'no-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              TYPE PREDICATES                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;.
;; Function: dtree?
;; 
;;   Dialog tree type predicate.
;;
;; Input: any value
;; Output: #t/#f
;; Side-effects: none
;.

(define dtree?
  (lambda (obj)
    (and (list? obj)
         (= (length obj) 4)
         (string? (car obj))
         (string? (cadr obj))
         (list? (caddr obj)))))


;.
;; Function: dtree-leaf?
;; 
;;   Dialog tree predicate that returns true if the tree is a leaf node.
;;
;; Input: any value
;; Output: #t/#f
;; Side-effects: none
;.

(define leaf-node?
  (lambda (obj)
    (and (dtree? obj) (null? (caddr obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 SELECTORS                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;.
;; Function: arc-label
;; 
;;   Returns the label of the arc that points to dtree.
;;
;; Input: A dialog tree
;; Output: The arc label (a string)
;; Side-effects: none
;.

(define arc-label 
  (lambda (dtree)

    ;; Make sure dtree is a dtree
    (if (not (dtree? dtree)) (error 'arc-label "Argument is not a dialog tree: " dtree))

    (car dtree)))

;.
;; Function: dtree-message
;; 
;;   Returns the text of the spoken dialog at the top of the dialog tree.
;;
;; Input: A dialog tree
;; Output: A string
;; Side-effects: none
;.

(define dtree-message
  (lambda (dtree)

    ;; Make sure dtree is a dtree
    (if (not (dtree? dtree)) (error 'dtree-message "Argument is not a dialog tree: " dtree))

    (cadr dtree)))

;.
;; Function: return-value
;; 
;;   Returns the return value of a leaf node.
;;
;; Input: A dialog tree leaf node
;; Output: (depends)
;; Side-effects: none
;.

(define return-value
  (lambda (leaf-node)

    ;; make sure leaf-node is a leaf-node
    (if (not (leaf-node? leaf-node)) 
        (error 'return-value "Argument is not a leaf node: " leaf-node))
    
    (list-ref leaf-node 3)))

;.
;; Function: list-choices
;; 
;;   Returns a list of all the arc labels for the children of dtree.
;;
;; Input: A dialog tree
;; Output: A list of strings
;; Side-effects: none
;.

(define list-choices 
  
  ;; First we need a recursive procedure that can find the arc labels
  ;; from a list of dtrees.
  (lambda (dtree)

    ;; Make sure dtree is a dtree
    (if (not (dtree? dtree)) (error 'list-choices "Argument is not a dialog tree: " dtree))
    
    (get-arc-labels (list-ref dtree 2))))

;.
;; Function: find-child
;; 
;;   Given a dialog tree and an arc label, returns the indicated child (or 
;;   #f if there is none).
;;
;; Input: A dialog tree and a string
;; Output: A dialog tree or #f
;; Side-effects: none
;.

(define find-child
  (lambda (dtree label)
    (find-node (caddr dtree) label)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             OTHER OPERATIONS                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;.
;; Function: converse
;; 
;;   "Executes" a conversation
;;
;; Input: A dialog tree
;; Output: The return value of the converstation
;; Side-effects: prints to the screen, requests input (indirectly)
;.

(define converse
  (lambda (dtree)
    (display-nl (dtree-message dtree))
    (cond 
      ((leaf-node? dtree) 
       (return-value dtree))
      (else
       (converse (find-child dtree (get-valid-choice (list-choices dtree))))))))
         



      



