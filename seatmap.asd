;;; seatmap.asd --- text-based venue and event management
;;; Snorre Svendsen <snorre.svendsen@gmail.com>
;;; Created: 2014-02-26
;;; Changed: 2016-02-25

(asdf:defsystem #:seatmap
  :serial t
  :description "seatmap - text-based venue and event management"
  :author "Snorre Svendsen <snorre.svendsen@gmail.com>"
  :license ""
  :depends-on (#:cl-who #:utility)
  :components ((:file "package")
	       (:file "globals")
	       (:file "venue")
               (:file "seatmap")
	       (:file "html")))
