(in-package #:dlist)

(defun dlist-nconc (&rest dlists)
  "Appends `dlists'. This works like `nconc' for singly-linked lists, except it is destructive and the resuld will share structure with the input dlists. This function should have running time proportional to the number of lists being appended."
  (unless dlists (return-from dlist-nconc dlists))
  (reduce #'(lambda (dlist1 dlist2)
	      (setf (next (dlist-last dlist1)) (dlist-first dlist2)
		    (prev (dlist-first dlist2)) (dlist-last dlist1)
		    (dlist-last dlist1) (dlist-last dlist2)) dlist1) dlists))

(defmacro dlist-push (obj dlist &key at-end)
  "Pushes `obj' onto `dlist'. If `at-end' is not-nil, the element is added to the end of dlist, otherwise it is added to the begining."
  (let ((obj-var (gensym)) (dlist-var (gensym)) (dlist-part-var (gensym)) (at-end-var (gensym)))
    `(let ((,at-end-var ,at-end) (,obj-var ,obj) (,dlist-var ,dlist))
       (if ,at-end-var
	   (if ,dlist-var
	       (let ((,dlist-part-var (dlist-last ,dlist-var)))
		 (setf (next ,dlist-part-var) (dcons ,dlist-part-var ,obj-var nil))
		 (setf (dlist-last ,dlist-var) (next ,dlist-part-var)) ,dlist-var)
	       (setf ,dlist (dlist ,obj-var)))
	   (if ,dlist-var
	       (let ((,dlist-part-var (dlist-first ,dlist-var)))
		 (setf (prev ,dlist-part-var) (dcons nil ,obj-var ,dlist-part-var))
		 (setf (dlist-first ,dlist-var) (prev ,dlist-part-var)) ,dlist-var)
	       (setf ,dlist (dlist ,obj-var)))))))

(defmacro dlist-pop (dlist &key from-end)
  "Pops an element from dlist and returns it. If `from-end' is non-`nil', the element will be popped from the end of the dlist. Otherwise, it will be popped from the begining."
  (let ((dlist-var (gensym)) (dlist-part-var (gensym)) (at-end-var (gensym)))
    `(let ((,at-end-var ,from-end))
       (if ,at-end-var
	   (let* ((,dlist-var ,dlist) (,dlist-part-var (dlist-last ,dlist)))
	     (if (and (not (prev ,dlist-part-var)) (not (next ,dlist-part-var)))
		 (setf ,dlist nil)
		 (progn
		   (setf (dlist-last ,dlist-var) (prev ,dlist-part-var))
		   (setf (next (dlist-last ,dlist)) nil)))
	     (data ,dlist-part-var))
	   (let* ((,dlist-var ,dlist) (,dlist-part-var (dlist-first ,dlist)))
	     (if (and (not (prev ,dlist-part-var)) (not (next ,dlist-part-var)))
		 (setf ,dlist nil)
		 (progn
		   (setf (dlist-first ,dlist-var) (next ,dlist-part-var))
		   (setf (prev (dlist-first ,dlist)) nil)))
	     (data ,dlist-part-var))))))

