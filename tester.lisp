
; CS340d Lab 2 definitions

; Below, you need to provide the definitions for the LIST-base
; functions:

;          insrt (e x)
;          mbr (e x)
;          del (e x)

; And for the TREE-based functions:

;          bst-insrt (e x)
;          bst-mbr (e x)
;          bst-del (e x)


; Sets with Lists and Binary Trees       Scott, Vivek, & Warren

; (ld "lab2-mem-insrt-del.lisp" :ld-pre-eval-print t)

(in-package "ACL2")

(defun << (x y)
  "General less-than function."
  (declare (xargs :guard t))
  (and (lexorder x y)
       (not (equal x y))))

; Ordered sets represented as lists.

(defun setp (x)
  "Ordered list of objects."
  (declare (xargs :guard t))
  (if (atom x)
      (null x)
    (if (atom (cdr x))
        (and (atom (car x))
             (null (cdr x)))
      (let ((a (car x))
            (b (cadr x)))
        (and (atom a)
             (atom b)
             (<< a b)
             (setp (cdr x)))))))

(defun insrt (e x)
  "Insert E into ordered set X."
  (declare (xargs :guard (and (atom e)
                              (setp x))))
  ;; Replace X (below) with an Insert function body
  (if (atom x)
      (list e)
    (let ((a (car x))
          (b (cadr x)))
      (if (or (equal e a)
              (equal e b))
          x
        (if (<< e a)
            (cons e x)
          (if (<< e b)
              (cons a (cons e (cdr x)))
            (cons a (cons b (insrt e (cddr x))))))))))

(defun mbr (e x)
  "Test whether E is in set X."
  (declare (xargs :guard (and (atom e)
                              (setp x))))
  ;; Replace form below with a Member function body
    (if (atom x)
        nil
        (let ((a (car x))
            (b (cadr x)))
        (or (equal e a)
            (equal e b)
            (mbr e (cddr x))))))

(defun del (e x)
  "Delete element from set X, or do nothing if no E in X."
  ;; Observation: When E "larger" than (CAR x), we can stop
  (declare (xargs :guard (and (atom e)
                              (setp x))))
  ;; Replace form below with a Delete function body.
    (if (atom x)
        x
        (let ((a (car x))
            (b (cadr x)))
        (if (equal e a)
            (if (equal e b)
                (cddr x)
                (cdr x))
            (if (equal e b)
                (cons a (cddr x))
            (if (<< e a)
                x
                (if (<< e b)
                    (cons a (del e (cddr x)))
                (cons a (cons b (del e (cddr x)))))))))))


; Ordered sets represented as trees.

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


(defun bst-insrt (e x)
  "BST insert element"
  (declare (xargs :guard (and (atom e)
                              (bstp x)
                              (bst-ordp x))))
    (if (atom x)
        (list e nil)
      (let ((obj (car x))
            (sbt (cdr x)))
        (cond ((<< e obj)
               (cons obj (cons (bst-insrt e (car sbt)) (cdr sbt))))
              ((<< obj e)
               (cons obj (cons (car sbt) (bst-insrt e (cdr sbt)))))
              (t
               x)))))


(defun bst-mbr (e x)
  "BST member, returns tree where e resides"
  (declare (xargs :guard (and (atom e)
                              (bstp x)
                              (bst-ordp x))))
  ;; Find if e is a member of a sorted binary search tree x using tr<<e and or e<<tr 
    (if (atom x)
        NIL
        (let ((obj (car x))
            (sbt (cdr x)))
        (if (equal e obj)
            t
            (if (<< e obj)
                (bst-mbr e (car sbt))
                (bst-mbr e (cdr sbt)))))))

;; Get leftmost object in a bst
(defun leftmost-obj (x)
  (declare (xargs :guard (bstp x)))
  (if (atom x)
      x
    (let ((obj (car x))
          (sbt (cdr x)))
      (if (atom (car sbt))
          obj
        (leftmost-obj (car sbt))))))

;; Copy a bst except for the given object e
(defun bst-copy (e x)
  (declare (xargs :guard (and (bstp x)
                              (bst-ordp x))))
  (if (atom x)
      x
    (let ((obj (car x))
          (sbt (cdr x)))
      (if (equal e obj)
          (cons (bst-copy e (car sbt)) (bst-copy e (cdr sbt)))
        (cons obj (cons (bst-copy e (car sbt)) (bst-copy e (cdr sbt))))))))

(defun bst-del (e x)
  "BST delete, if element e present, delete it"
  (declare (xargs :guard (and (bstp x)
                              (bst-ordp x))))
  ;; Delete e from x if it is present inside the sorted binary tree using 
  ;; (bst-del 5 '(5 (3 (1 ())) 9 (6 ()) 10 ())) would be (6 (3 (1 NIL)) 9 NIL 10 NIL)
    (if (atom x)
        x
        (let ((obj (car x))
            (sbt (cdr x)))
        (if (equal e obj)
            (if (atom (car sbt))
                (cdr sbt)
                (if (atom (cdr sbt))
                    (car sbt)
                    (cons (leftmost-obj (cdr sbt)) (bst-copy (leftmost-obj (cdr sbt)) x))))
            (if (<< e obj)
                (cons obj (cons (bst-del e (car sbt)) (cdr sbt)))
                (cons obj (cons (car sbt) (bst-del e (cdr sbt)))))))))

