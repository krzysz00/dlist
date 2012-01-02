;; Copyright (c) 2011-2012, Krzysztof Drewniak
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <organization> nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

