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
    (assert-equal (dlist-first dlist) (prev (dlist-last dlist)))))

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

(define-test conversion
  (assert-equal '(1 2 3) (dlist->list (dlist 1 2 3)))
  (assert-equality #'dlist= (dlist 1 2 3) (apply #'dlist '(1 2 3))))

(define-test append
  (assert-equality #'dlist= (dlist 1 2 3 4 5 6) (dlist-append (dlist 1 2 3) (dlist 4 5 6)))
  (assert-equality #'dlist= (dlist 1 2 3 #\a #\b #\c) (dlist-append (dlist 1 2 3) (dlist #\a #\b #\c)))
  (assert-equal (dlist-append) nil)
  (assert-equality #'dlist= (dlist-append (dlist 1 2 3)) (dlist 1 2 3))
  (assert-equality #'dlist= (dlist 1 2 3 4 5 6) (dlist-append (dlist 1 2) (dlist 3 4) (dlist 5 6))))

(define-test length
  (assert-equal 0 (dlist-length nil))
  (assert-equal 1 (dlist-length (dlist 1)))
  (assert-equal 2 (dlist-length (dlist 1 2))))

(define-test nth
  (let ((dlist (dlist 1 2 3 4)))
    (assert-equal 1 (dlist-nth 0 dlist))
    (assert-equal 3 (dlist-nth 2 dlist))
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
    (assert-equality #'dlist= (dlist 1 2 3) dlist)))

;;Compatibility magic so we can reference lisp-unit macros to run tests
(defun %run-tests ()
  (lisp-unit:run-all-tests :dlist))

(defmethod asdf:perform ((o asdf:test-op) (system (eql (asdf:find-system 'dlist-test))))
  (%run-tests))
