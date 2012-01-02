(in-package #:dlist)

(defun dlist-reverse (dlist)
  "Reverses dlist non-destructively."
  (mapdlist #'(lambda (x) x) dlist :from-end t))

(defun copy-dlist (dlist &key deep-copy)
  "Copies `dlist', returning a new dlist with the same elements as `dlist'. If `deep-copy' is true, `copy-dlist' deep-copies dlists and sequences."
  (mapdlist #'(lambda (x) (cond 
			    ((and deep-copy (or (dlistp x) (dconsp x))) (copy-dlist x))
			    ((and deep-copy (typep x 'sequence)) (copy-seq x))
			    (t x))) dlist))

(defun make-dlist (size &key initial-element)
  "Creates a dlist that contains `initial-element' `size' times."
  (let ((ret nil))
    (loop repeat size do (dlist-push initial-element ret)) ret))

(defun dlist-length (dlist)
  "Returns the length of `dlist'"
  (let ((ret 0))
    (dodlist (i dlist ret)
      (declare (ignore i))
      (incf ret))))

(defun dlist-append (&rest dlists)
  "Appends `dlists' non-derstructively by calling `dlist-nconc' with shallow copies of each dlist."
  (apply #'dlist-nconc (mapcar #'copy-dlist dlists))) 
