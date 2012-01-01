(in-package #:dlist)

(defmacro dodcons ((var dlist &optional result-form from-end) &body body)
  "Loops over the dconses in `dlist', binding `var' to each in turn. If `from-end' is non-nil, the loop proceeds from the last element of ther list to the first. This is basically `dolist' for dlists."
  (let ((dlist-var (gensym)) (from-end-var (gensym)))
    `(let* ((,dlist-var ,dlist) (,from-end-var ,from-end))
       (if ,from-end-var
	   (do ((,var (dlist-last ,dlist-var) (prev ,var)))
	       ((eql ,var nil) ,result-form)
	     ,@body)
	   (do ((,var (dlist-first ,dlist-var) (next ,var)))
	       ((eql ,var nil) ,result-form)
	     ,@body)))))

(defmacro dodlist ((var dlist &optional result-form from-end) &body body)
  "Loops over the elements in `dlist', binding each to `var' in turn, then executing `body'. If `from-end' is non-nil, the loop proceeds from the end of the list to the begining."
  (let ((dcons-var (gensym)))
    `(dodcons (,dcons-var ,dlist ,result-form ,from-end)
       (let ((,var (data ,dcons-var)))
	 ,@body))))

(defun mapdcons (function dlist &rest more-dlists-and-from-end)
  "Maps over the dconses in `dlist' and `more-dlists'. If `more-dlists' contains the keyword :from-end, the value after it in the argumnt list will be taken as the value of :from-end, and both will be removed from `more-dlists'. The order of elements in the result is the same as the oder in which the elements are returned from the function."
  (let ((from-end nil) (more-dlists more-dlists-and-from-end))
    (let ((it (member :from-end more-dlists)))
      (when it
	(setf from-end (car (cdr it)))
	(setf more-dlists (remove-if #'(lambda (x) (declare (ignore x)) t) more-dlists 
				     :start (position :from-end more-dlists) :count 2))))
    (if (not more-dlists)
	(let ((ret nil))
	  (dodcons (i dlist nil from-end)
	    (dlist-push (funcall function i) ret :at-end t))
	  ret)
	(let ((dlists (mapcar (if from-end #'dlist-last #'dlist-first) (cons dlist more-dlists)))
	      (ret nil))
	  (loop
	     (when (some #'not dlists) (return))
	     (dlist-push (apply function dlists) ret :at-end t)
	     (setf dlists (mapcar (if from-end #'prev #'next) dlists)))
	  ret))))

(defun mapdlist (function dlist &rest more-dlists-and-from-end)
  "Behaves like `mapdcons', except the function will be passed the `data' of each dcons."
  (apply #'mapdcons #'(lambda (&rest i) (apply function (mapcar #'data i))) dlist more-dlists-and-from-end))

(defun mapdcon (function dlist &rest more-dlists-and-from-end)
  "Maps `function' over `dlist' the dconses in `dlist', then returns `dlist'"
  (apply #'mapdcons #'(lambda (&rest args) (apply function args)) dlist more-dlists-and-from-end)
  dlist)
