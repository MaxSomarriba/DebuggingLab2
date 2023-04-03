; Ordered sets represented as trees.

(defun << (x y)
  "General less-than function."
  (declare (xargs :guard t))
  (and (lexorder x y)
       (not (equal x y))))

(defun bstp (x)
  "Syntactic Tree Set Recognizer."
  (declare (xargs :guard t))
  ;; If NIL, x is the empty bst
  (if (atom x)
      (null x)
    (let ((obj (car x))
          (sbt (cdr x)))
      (and (atom obj)
           (consp sbt)
           (bstp (car sbt))
           (bstp (cdr sbt))))))

(defun tr<<e (x e)
  "All elements in X less than e."
  (declare (xargs :guard (bstp x)))
  (if (atom x)
      T
    (let ((obj (car x))
          (sbt (cdr x)))
      (and (<< obj e)
           (tr<<e (car sbt) e)
           (tr<<e (cdr sbt) e)))))

(defun e<<tr (e x)
  "All elements in X greater than e."
  (declare (xargs :guard (bstp x)))
  (if (atom x)
      T
    (let ((obj (car x))
          (sbt (cdr x)))
      (and (<< e obj)
           (e<<tr e (car sbt))
           (e<<tr e (cdr sbt))))))

(defun bst-ordp (x)
  "Recognizer for tree-based sets; all elements ordered."
  (declare (xargs :guard (bstp x)))
  (if (atom x)
      t
    (let* ((obj (car x))
           (sbt (cdr x))
           (lt (car sbt))
           (rt (cdr sbt)))
      ;; Consider both subtrees
      (and (bst-ordp lt)
           (bst-ordp rt)
           ;; Confirm that values "surround" OBJ
           (tr<<e lt obj)
           (e<<tr obj rt)))))


;; Delete function for BSTs 
;; (bst-del 5 '(5 (3 (1 ())) 9 (6 ()) 10 ())) should be (6 (3 (1 NIL)) 9 NIL 10 NIL)
;; (bst-del 3 '(5 (3 (1 ())) 9 (6 ()) 10 ())) should be (5 (1 NIL) 9 (6 NIL) 10 NIL)
(defun bst-del (e x)
  "BST delete, if element e present, delete it"
  (declare (xargs :guard (and (bstp x)
                              (bst-ordp x))))
    (if (atom x)
        x
      (let ((obj (car x))
            (sbt (cdr x)))
        (cond ((<< e obj) (cons obj (cons (bst-del e (car sbt)) (cdr sbt))))
              ((<< obj e) (cons obj (cons (car sbt) (bst-del e (cdr sbt)))))
              (t (cons (car sbt) (cdr sbt)))))))
        

(defun bst-insrt (e x)
  (cond ((null x) `(,e nil nil))
        ((<< e (car x))
         (cons (car x)
               (list (bst-insrt e (cadr x)) (caddr x))))
        ((<< (car x) e)
         (cons (car x)
               (list (cadr x) (bst-insrt e (caddr x)))))
        (t x)))
        


(bst-insrt 4 '(5 (3 (1 ())) 9 (6 ()) 10 ()))
(((3 (1 NIL)) 9 (6 NIL) 10 NIL)
 ((1 NIL))
 . 4)


((((3 (1 NIL)) 9 (6 NIL) 10 NIL)
  (((1 NIL)) . 4)))


(defun bst-insrt (e x)
  "BST insert element"
  (declare (xargs :guard (and (atom e)
                              (bstp x)
                              (bst-ordp x))))
    (if (atom x)
        (list e)
      (let ((obj (car x))
            (sbt (cdr x)))
        (if (<< e obj)
            (list obj (bst-insrt e (car sbt)) (cdr sbt))
          (list obj (car sbt) (bst-insrt e (cdr sbt)))))))


(defun bst-insrt (e x)
  "BST insert element"
  (declare (xargs :guard (and (atom e)
                              (bstp x)
                              (bst-ordp x))))
    (if (atom x)
        (list e)
      (let ((obj (car x))
            (sbt (cdr x)))
        (cond ((<< e obj)
               (list obj (bst-insrt e (car sbt)) (cdr sbt)))
              ((<< obj e)
               (list obj (car sbt) (bst-insrt e (cdr sbt))))
              (t
               x)))))