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

(defstruct dcons 
  "A three-member cons cell for doubly-linked lists, which has `prev', `data' and `next' slots" 
  prev data next)

(defun dcons (prev data next) 
  "Constructs a `dcons' with the given `prev', `data', and `next'"
  (make-dcons :prev prev :data data :next next))

(defun next (dcons)
  "Accesses the `next' slot of a dcons. The `next' of nil is nil." 
  (if dcons (dcons-next dcons)))

(defun prev (dcons) 
  "Accesses the `prev' slot of a dcons. The `prev' of nil is nil."
  (if dcons (dcons-prev dcons)))

(defun data (dcons) 
  "Accesses the `data' slot of a dcons. The `data' of nil is nil."
  (if dcons (dcons-data dcons)))

(defun (setf next) (val place)
  "Sets the `next' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-next place) val))

(defun (setf prev) (val place)
  "Sets the `prev' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-prev place) val))

(defun (setf data) (val place)
  "Sets the `data' slot of `place' (which must be a `dcons') to `val'"
  (setf (dcons-data place) val))

(declaim (inline dconsp))
(defun dconsp (object)
  "Returns T if `object' is a dcons"
  (typep object 'dcons))
