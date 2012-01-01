(in-package #:dlist)

(defun dlist-reverse (dlist)
  "Reverses dlist non-destructively."
  (mapdlist #'(lambda (x) x) dlist :from-end t))

(defun copy-dlist (dlist &key deep-copy)
  "Copies `dlist', returning a new dlist with the same elements as `dlist'. If `deep-copy' is true, `copy-dlist' deep-copies dlists and sequences."
  (mapdlist #'(lambda (x) (cond 
			    ((and deep-copy (or (dlistp x) (dconsp x))) (copy-dlist x))
			    ((and deep-copy (typep x 'sequence)) (copy-seq x))
			    (t x)))))

