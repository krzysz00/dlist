(in-package #:dlist)

#+generic-sequences
(progn
  (defmethod sequence:length ((x dlist))
    (dlist-length x))

  (defmethod sequence:elt ((x dlist) n)
    (dlist-nth n x))

  (defmethod (setf sequence:elt) (value (seq dlist) i)
    (setf (dlist-nth i seq) value))
  
  (defmethod sequence:adjust-sequence 
      ((seq dlist) length &key initial-element (initial-contents nil ic-sp))
    (let ((delta (- (dlist-length seq) length)))
      (loop repeat delta do (dlist-pop seq :from-end t))
      (cond
	((= length 0) nil)
	(ic-sp
	 (loop 
	    repeat length
	    for i = initial-contents then (cdr i)
	    for j = (dlist-first seq) then (next j)
	    while i do
	      (if j (setf (data j) (car i)) (dlist-push (car i) seq :at-end t))))
	(t
	 (loop repeat delta do
	      (dlist-push initial-element seq :at-end t))))
      seq))
  
  (defmethod sequence:make-sequence-like ((seq dlist) length &key initial-element initial-contents)
    (cond
      ((= length 0) nil)
      (initial-contents (apply #'dlist (coerce initial-contents 'list)))
      (t (make-dlist length :initial-element initial-element))))

  (defmethod sequence:reverse ((seq dlist))
    (dlist-reverse seq))

  (defmethod sequence:make-sequence-iterator ((seq dlist) &key from-end (start 0) end)
    (let ((begin-val
	   (if from-end
	       (if end 
		   (nthdcons (1- (- (dlist-length seq) end)) seq :from-end t)
		   (dlist-last seq))
	       (if start
		   (nthdcons start seq)
		   (dlist-first seq)))))
      (values 
	   begin-val ;;iterator
	   (if from-end ;;limit
	       (if (> start 0)
		   (nthdcons (1- start) seq)
		   nil)
	       (if end
		   (nthdcons end seq)
		   nil))
	   from-end ;;from-end
	   #'sequence:iterator-step
	   #'sequence:iterator-endp
	   #'sequence:iterator-element
	   #'(setf sequence:iterator-element)
	   #'sequence:iterator-index
	   #'sequence:iterator-copy)))

  (defmethod sequence:iterator-step ((seq dlist) iter from-end)
    (funcall (if from-end #'prev #'next) iter))

  (defmethod sequence:iterator-endp ((seq dlist) iter lim (from-end t))
    (eql iter lim))

  (defmethod sequence:iterator-element ((seq dlist) iter)
    (data iter))

  (defmethod (setf sequence:iterator-element) (val (seq dlist) iter)
    (setf (data iter) val))

  (defmethod sequence:iterator-index ((seq dlist) iter)
    (let ((ret 0))
      (dodcons (i seq ret)
	(when (eql iter i) (return ret))
	(incf ret))))

  );; end progn