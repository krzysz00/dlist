(in-package #:dlist)

(defstruct dcons 
  "A three-member cons cell for doubly-linked lists, which has `prev', `data' and `next' slots" 
  prev data next)

(defun dcons (prev data next) 
  "Constructs a `dcons' with the goven `prev', `data', and `next'"
  (make-dcons :prev prev :data data :next next))

(defun next (dcons)
  "Accesses the `next' slot of a dcons. The `next' of nil is nil." 
  (if dcons (dcons-next dcons)))

(defun prev (dcons) 
  "Accesses the `prev' slot of a dcons. The `prev' of nil is nil."
  (if dcons (dcons-prev dcons)))

(defun data (dcons) 
  "Accesses the `data' slot of a dcons. The `data' of nil is nil."
  (if dcons (dcons-data dcons)))

(defun (setf next) (val place)
  "Sets the `next' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-next place) val))

(defun (setf prev) (val place)
  "Sets the `prev' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-prev place) val))

(defun (setf data) (val place)
  "Sets the `data' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-data place) val))

(declaim (inline dconsp))
(defun dconsp (object)
  "Returns T if `object' is a dcons"
  (typep object 'dcons))

