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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :lisp-unit))

(define-test dcons-creation 
  (let ((dcons (dcons 1 2 3)))
    (assert-true dcons)
    (assert-equal (prev dcons) 1)
    (assert-equal (data dcons) 2)
    (assert-equal (next dcons) 3)))

(define-test dcons-modification
  (let ((dcons (dcons 1 2 3)))
    (setf (prev dcons) "foo" (data dcons) 6 (next dcons) t)
    (assert-equal (prev dcons) "foo")
    (assert-equal (data dcons) 6)
    (assert-equal (next dcons) t)))

(define-test dlist-creation
  (let ((dlist (dlist 1 2)))
    (assert-equal 1 (data (dlist-first dlist)))
    (assert-equal 2 (data (dlist-last dlist)))
    (assert-equal (dlist-first dlist) (prev (dlist-last dlist))))
  (let ((dlist (dlist 1)))
    (assert-equal (dlist-first dlist) (dlist-last dlist))))

(define-test equality
  (assert-true (dlist= (dlist 1 2 3) (dlist 1 2 3)))
  (assert-true (dlist= (dlist 1 2 3)))
  (assert-true (dlist= (dlist 1 2 3) (dlist 1 2 3) (dlist 1 2 3)))
  (assert-true (dlist= (dlist 1 (dlist 2 3)) (dlist 1 (dlist 2 3))))
  (assert-false (dlist= (dlist 1 2 3) (dlist 1 2)))
  (assert-false (dlist= (dlist 1 2 3) (dlist 4 5 6)))
  (assert-false (dlist= (dlist (dlist 1 2) 3) (dlist 1 (dlist 2 3))))
  (assert-true (dlist= nil nil))
  (assert-true (dlist= nil nil nil))
  (assert-true (dlist= nil))
  (assert-false (dlist= nil (dlist 1)))
  (assert-true (dlist= (dlist 1) (dlist 1))))

(define-test make-dlist
  (assert-equality #'dlist= (dlist nil nil nil nil) (make-dlist 4))
  (assert-equality #'dlist= (dlist 2 2 2 2) (make-dlist 4 :initial-element 2)))

(define-test conversion
  (assert-equal '(1 2 3) (dlist->list (dlist 1 2 3)))
  (assert-equality #'dlist= (dlist 1 2 3) (apply #'dlist '(1 2 3))))

(define-test append
  (loop for f in (list #'dlist-nconc #'dlist-append) do
       (assert-equality #'dlist= (dlist 1 2 3 4 5 6) (funcall f (dlist 1 2 3) (dlist 4 5 6)))
       (assert-equality #'dlist= (dlist 1 2 3 #\a #\b #\c) (funcall f (dlist 1 2 3) (dlist #\a #\b #\c)))
       (assert-equal (funcall f) nil)
       (assert-equality #'dlist= (funcall f (dlist 1 2 3)) (dlist 1 2 3))
       (assert-equality #'dlist= (dlist 1 2 3 4 5 6) (funcall f (dlist 1 2) (dlist 3 4) (dlist 5 6)))))

(define-test length
  (assert-equal 0 (dlist-length nil))
  (assert-equal 1 (dlist-length (dlist 1)))
  (assert-equal 2 (dlist-length (dlist 1 2))))

(define-test nth
  (let ((dlist (dlist 1 2 3 4)))
    (assert-equal 1 (dlist-nth 0 dlist))
    (assert-equal 3 (dlist-nth 2 dlist))
    (assert-equal 4 (dlist-nth 0 dlist :from-end t))
    (assert-equal '(nil nil) (multiple-value-list (dlist-nth 3 nil)))
    (assert-equal '(nil nil) (multiple-value-list (dlist-nth 7 dlist)))
    (assert-equal (dlist-first dlist) (nthdcons 0 dlist))
    (assert-equal (next (next (dlist-first dlist))) (nthdcons 2 dlist))
    (assert-equal nil (nthdcons 4 dlist))
    (setf (dlist-nth 1 dlist) 5)
    (assert-equal 5 (data (next (dlist-first dlist))))
    (assert-equal 5 (dlist-nth 1 dlist))))

(define-test push-pop
  (let ((dlist (dlist 1 2 3)))
    (dlist-push 0 dlist)
    (dlist-push 4 dlist :at-end t)
    (assert-equality #'dlist= dlist (dlist 0 1 2 3 4))
    (assert-equal 0 (dlist-pop dlist))
    (assert-equal 4 (dlist-pop dlist :from-end t))
    (assert-equality #'dlist= (dlist 1 2 3) dlist)
    (setf dlist nil)
    (dlist-push 1 dlist)
    (dlist-push 2 dlist)
    (dlist-push 3 dlist)
    (assert-equality #'dlist= (dlist 3 2 1) dlist)
    (loop repeat 3 do (dlist-pop dlist :from-end t))
    (assert-equal nil dlist)))

(define-test do-macros
  (let ((dlist (dlist 1 2 3))
	(ret nil))
    (dodcons (i dlist)
      (push (data i) ret))
    (assert-equal '(3 2 1) ret)
    (setf ret nil)
    (dodlist (i dlist nil t)
      (push i ret))
    (assert-equal '(1 2 3) ret)))

(define-test mappers
  (assert-equality #'dlist= (dlist 2 3 4 nil) (mapdcons #'(lambda (x) (data (next x))) (dlist 1 2 3 4)))
  (assert-equality #'dlist= (dlist nil 4 3 2) 
		   (mapdcons #'(lambda (x) (data (next x))) (dlist 1 2 3 4) :from-end t))
  (assert-equality #'dlist= (dlist 12 9 6) (mapdlist #'+ (dlist 1 2 3) (dlist 2 3 4) (dlist 3 4 5) :from-end t))
  (flet ((my-position (object dlist)
	   (let ((position 0))
	     (mapdlist #'(lambda (x) (when (equal x object) (return-from my-position position)) (incf position))
		       dlist) nil)))
    (assert-equal nil (my-position 1 nil))
    (assert-equal 0 (my-position 1 (dlist 1 2 3)))
    (assert-equal 2 (my-position 3 (dlist 1 2 3)))
    (assert-equal nil (my-position 4 (dlist 1 2 3)))))

(define-test reverse
  (assert-equality #'dlist= (dlist 3 2 1) (dlist-reverse (dlist 1 2 3))))

(define-test copy-dlist
  (let ((dlist (dlist 1 2 3)))
    (assert-false (eql dlist (copy-dlist dlist)))))

#+generic-sequences
(define-test generic-seqs
  (assert-equality 
   #'dlist=
   (dlist 1 2 3 4 5 5 5 5)
   (nsubstitute 5 4 (dlist 1 2 3 4 5 4 5 4) :count 2 :from-end t))
  (assert-equality
   #'dlist=
   (dlist 1 2 3 5 5 5 5 4)
   (substitute 5 4 (dlist 1 2 3 4 5 4 5 4) :count 2))
  (assert-equal 3 (elt (dlist 1 2 3 4) 2))
  (assert-equal '(1 2 3) (coerce (dlist 1 2 3) 'list))
  (assert-equality #'dlist= (dlist 1 2 3) (coerce #(1 2 3) 'dlist))
  (assert-equality #'dlist= (dlist 1 2 3 4) (sequence:adjust-sequence (dlist 1 2 3 4 5) 4))
  (assert-equality #'dlist= (dlist 1 2 3 4 5 nil) 
		   (sequence:adjust-sequence (dlist 1 2 3 4 5) 6 :initial-element nil))
  (assert-equality #'dlist= (dlist 6 7 8 4)
		   (sequence:adjust-sequence (dlist 1 2 3 4) 4 :initial-contents '(6 7 8)))
  (assert-equality #'dlist= (dlist nil nil) (sequence:make-sequence-like (dlist 1 2 3) 2))
  (assert-equal "def" (map 'string #'code-char (dlist 100 101 102)))
  (assert-equality #'dlist= (dlist 3 4) 
		   (sequence:make-sequence-like (dlist 1 2 3 4) 2 :initial-contents '(3 4)))
  (assert-equality #'dlist= (dlist 5 5 5) (sequence:make-sequence-like (dlist 1) 3 :initial-element 5))
  (assert-equality #'dlist= (dlist 1 2 3) (delete-duplicates (dlist 1 1 2 2 3))))

;;Compatibility magic so we can reference lisp-unit macros to run tests
(defun %run-tests ()
  (lisp-unit:run-all-tests :dlist))

(defmethod asdf:perform ((o asdf:test-op) (system (eql (asdf:find-system 'dlist-test))))
  (%run-tests))

