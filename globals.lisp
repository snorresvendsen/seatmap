;;;; globals.lisp
;;;; snorre.svendsen@gmail.com
;;;; Created 2014-05-21

(in-package #:seatmap)

(defparameter *current-event*
  (make-hash-table)
  "Default hash table for seat references. Change with set-event.")

(defparameter *current-venue*
  nil
  "Holds current venue list for easy reference.")

