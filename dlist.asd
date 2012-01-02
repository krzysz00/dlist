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

(defpackage #:dlist-system
  (:use :asdf :cl))

(defsystem #:dlist
  :name "dlist"
  :author "Krzyszxtof Drewniak <krzysdrewniak@gmail.com>"
  :license "3-Clause BSD"
  :description "An implementation of the doubly-linked list in Common Lisp."
  :in-order-to ((test-op (test-op #:dlist-test)))
  :serial t
  :components ((:file "package")
               (:file "dcons")
	       (:file "dlist")
	       (:file "modification")
	       (:file "mapping")
	       (:file "more-ops")
	       (:file "generic-sequences")
	       (:file "iterate")))

(defsystem #:dlist-test
  :depends-on (:dlist :lisp-unit)
  :name "dlist-test"
  :author "Krzyszxtof Drewniak <krzysdrewniak@gmail.com>"
  :license "3-Clause BSD"
  :description "The test suite for dlist."
  :serial t
  :components ((:file "tests")))
