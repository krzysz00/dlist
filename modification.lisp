(in-package #:dlist)

(defun dlist-append (&rest dlists)
  "Appends `dlists'. This works like `append' for singly-linked lists, except it is destructive and the resuld will share structure with the input dlists."
  (unless dlists (return-from dlist-append dlists))
  (reduce #'(lambda (dlist1 dlist2)
	      (setf (next (dlist-last dlist1)) (dlist-first dlist2)
		    (prev (dlist-first dlist2)) (dlist-last dlist1)
		    (dlist-last dlist1) (dlist-last dlist2)) dlist1) dlists))

(defmacro dlist-push (obj dlist &key at-end)
  "Pushes `obj' onto `dlist'. If `at-end' is not-nil, the element is added to the end of dlist, otherwise it is added to the begining."
  (let ((obj-var (gensym)) (dlist-var (gensym)) (dlist-part-var (gensym)) (at-end-p at-end))
    (if at-end-p
	`(let* ((,obj-var ,obj) (,dlist-var ,dlist) (,dlist-part-var (dlist-last ,dlist-var)))
	   (setf (next ,dlist-part-var) (dcons ,dlist-part-var ,obj-var nil))
	   (setf (dlist-last ,dlist-var) (next ,dlist-part-var)) ,dlist-var)
	`(let* ((,obj-var ,obj) (,dlist-var ,dlist) (,dlist-part-var (dlist-first ,dlist-var)))
	   (setf (prev ,dlist-part-var) (dcons nil ,obj-var ,dlist-part-var))
	   (setf (dlist-first ,dlist-var) (prev ,dlist-part-var)) ,dlist-var))))

(defmacro dlist-pop (dlist &key from-end)
  "Pops an element from dlist and returns it. If `from-end' is non-`nil', the element will be popped from the end of the dlist. Otherwise, it will be popped from the begining."
  (let ((dlist-var (gensym)) (dlist-part-var (gensym)) (at-end-p from-end))
    (if at-end-p
	`(let* ((,dlist-var ,dlist) (,dlist-part-var (dlist-last ,dlist)))
	   (setf (dlist-last ,dlist-var) (prev ,dlist-part-var))
	   (setf (next (dlist-last ,dlist)) nil)
	   (data ,dlist-part-var))
	`(let* ((,dlist-var ,dlist) (,dlist-part-var (dlist-first ,dlist)))
	   (setf (dlist-first ,dlist-var) (next ,dlist-part-var))
	   (setf (prev (dlist-first ,dlist)) nil)
	   (data ,dlist-part-var)))))

