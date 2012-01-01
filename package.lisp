(defpackage #:dlist
  (:use #:cl)
  (:export
   #:dcons
   #:dlist
   #:dlist-first
   #:dlist-last
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
   #:dlist-push
   #:dlist-pop
   #:mapdlist
   #:mapdcons
   #:mapdcon
   #:dlist-reverse
   #:dodlist
   #:dodcons
   #:copy-dlist
   ))
