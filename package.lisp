;;; package.lisp --- seatmap defpackage
;;; Snorre Svendsen <snorre.svendsen@gmail.com>
;;; Created: 2015-02-26
;;; Changed: 2016-02-24

(defpackage #:seatmap
  (:nicknames #:sm)
  (:use #:cl #:utility)
  (:shadowing-import-from #:cl-who
                          #:with-html-output
                          #:htm
                          #:str
                          #:fmt
                          #:conc)
  (:export #:load-venue
	   #:print-event
           #:print-venue
	   #:set-seats
           #:set-rows
           #:list-seats
           #:set-seat-list
           #:count-seats
           #:change-seats
           #:list-values
	   #:print-report))
