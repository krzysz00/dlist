(defpackage #:dlist-system
  (:use :asdf :cl))

(defsystem #:dlist
  :name "dlist"
  :author "Krzyszxtof Drewniak <krzysdrewniak@gmail.com>"
  :license "3-Clause BSD"
  :description "An implementation of the doubly-linked list in Common Lisp."
  :depends-on (:iterate)
  :in-order-to ((test-op (test-op #:dlist-test)))
  :serial t
  :components ((:file "package")
               (:file "dcons")
	       (:file "dlist")
	       (:file "modification")))

(defsystem #:dlist-test
  :depends-on (:dlist :lisp-unit)
  :name "dlist-test"
  :author "Krzyszxtof Drewniak <krzysdrewniak@gmail.com>"
  :license "3-Clause BSD"
  :description "The test suite for dlist."
  :serial t
  :components ((:file "tests")))
