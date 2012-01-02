(defpackage #:dlist
  (:use #:cl)
  (:export
   #:dcons
   #:dlist
   #:dlist-first
   #:dlist-last
   #:make-dlist
   #:next
   #:prev
   #:data
   #:dlist=
   #:dconsp
   #:dlistp
   #:dlist-length
   #:nthdcons
   #:dlist-nth
   #:dlist->list
   #:dlist-append
   #:dlist-nconc
   #:dlist-push
   #:dlist-pop
   #:mapdlist
   #:mapdcons
   #:mapdcon
   #:dlist-reverse
   #:dodlist
   #:dodcons
   #:copy-dlist
   #:in-dlist
   #:on-dlist
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sequence) (pushnew :generic-sequences *features*)))
