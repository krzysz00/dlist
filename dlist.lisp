(in-package #:dlist)

(defstruct (dlist (:conc-name %dlist-))
  "A doubly-linked list"
  first last)

(defun dlist-first (dlist)
  "Get the first `dcons' in a `dlist'"
  (if dlist (%dlist-first dlist)))

(defun dlist-last (dlist)
  "Get the last `dcons' in a `dlist'"
  (if dlist (%dlist-last dlist)))

(defun (setf dlist-first) (val place)
  (setf (%dlist-first place) val))

(defun (setf dlist-last) (val place)
  (setf (%dlist-last place) val))

(defun dlist-cons-on (object dlist)
  "Returns a dlist whose elements are `object' and the elements of `dlist'. `dlist' is destructively mosified. This is intended to have the same use as `(cons object list)' for regular lists."
  (let ((new-cons (dcons nil object dlist)))
    (setf (prev dlist) new-cons)
    new-cons))

(defun dcons-append (object dcons)
  "Creates a dcons whose `data' is `object' and appends it to `dcons', returning `dcons' with a pointer to the new dcons in `dcons''s next."
  (let ((new-dcons (dcons dcons object nil)))
    (setf (next dcons) new-dcons)
    dcons))

(defun dlist (&rest elements)
  "Return a doubly-linked list (dlist) with the elements in `elements'"
  (when elements
    (let ((dlist (make-dlist)) (current-dcons nil))
      (setf (dlist-first dlist) (dcons nil (first elements) nil))
      (setf current-dcons (dlist-first dlist))
      (loop for i on (rest elements) do
	   (setf current-dcons (next (dcons-append (car i) current-dcons)))
	   (or (cdr i) (setf (dlist-last dlist) current-dcons)))
      dlist)))

(defun dlist= (dlist &rest more-dlists)
  (unless more-dlists (return-from dlist= t))
  (if (cdr more-dlists) ;;Test for a list of length > 1
      (every #'(lambda (x) (dlist= dlist x)) more-dlists)
      (loop for i = (dlist-first dlist) then (next i)
	   for j = (dlist-first (first more-dlists)) then (next j)
	   until (and (eql i nil) (eql j nil))
	   always
	   (if (and (typep (data i) 'dlist) (typep (data j) 'dlist))
	       (dlist= (data i) (data j))
	       (equal (data i) (data j))))))

(defun dlist->list (dlist)
  "Convert a dlist to a list"
  (loop for i = (dlist-first dlist) then (next i) while i collect (data i)))
