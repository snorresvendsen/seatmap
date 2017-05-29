;;;; html.lisp
;;;; snorre.svendsen@gmail.com
;;;; Created 2014-08-24
;;;; Changed 2016-11-20

;;;; Part of package "seatmap"

;;;; Routines for printing html code.

;;; Last change: New names for global vars.

(in-package #:seatmap)

(defparameter *include-directive-start* "<!-- #include \"")
(defparameter *include-directive-end* "\" -->")

(defun html-print-venue (&optional (venue-list *current-venue*))
  "Prints venue as an HTML table."
  (with-html-output (*standard-output* nil)
    (:table
     (dolist (row-list venue-list)
       (htm (:tr :id (getf row-list :lbl)
                 (:td :class "label"
                      (str (getf row-list :lbl)))
                 (dolist (seat-str (getf row-list :seats))
                   (if (equal seat-str ".")
                       (htm (:td :class "invisible"))
                       (htm (:td :class "unselected"
                                 :id (conc "R" (getf row-list :lbl) ".P" seat-str)
                                 :onclick "getID(this);"
                                 (str seat-str)))))))))))

(defun html-print-event (&key (table-width nil) (event-hash *current-event*)
                           (venue-list *current-venue*))
  "Print event as an HTML table. Optionally set width to TABLE-WIDTH px."
  (with-html-output (*standard-output* nil)
    (:table :width table-width :align "left"
     (dolist (row-list venue-list)
       (let ((row-lbl (getf row-list :lbl)))
         (htm (:tr :id row-lbl)
                   (:td :class "label"
                        (str row-lbl))
                   (dolist (seat-str (getf row-list :seats))
                     (if (equal seat-str ".")
                         (htm (:td :class "invisible"))
                         (let ((seat-value (get-seat
                                            (intern (conc "R" row-lbl ".P" seat-str))
                                            event-hash)))
                           (if (eql 0 seat-value)
                               (htm (:td :class "unselected"
                                         :id (conc "R" row-lbl ".P" seat-str)
                                         :onclick "getID(this);"
                                         (str seat-str)))
                               ;; Kludge: Add "N" to class names starting with numbers
                               ;;   in seat-value; CSS does not allow class '1'.
                               (htm (:td :class (if (numberp seat-value) (conc "N" (write-to-string seat-value)) (write-to-string seat-value))
                                         :id (conc "R" row-lbl ".P" seat-str)
                                         :onclick "getID(this);"
                                         (str seat-str)))))))))))))

(defun html-build-event-file (fname &optional (event-hash *current-event*))
  "Build html file with name FNAME in current directory. Presupposes that the file seatmap-template.html exists in current directory."
  ;; To do:
  ;; Presupposes a file infrastructure. In current directory:
  ;; seatmap-template.html
  ;; which may reference other files,
  ;; headings.html
  ;; seatmap.css seatmap.js
  (with-open-file (*standard-output* "table.html" :direction :output :if-exists :supersede)
    (html-print-event :table-width 1360 :event-hash event-hash))
  (with-open-file (*standard-output* fname :direction :output :if-exists :supersede)
    (html-include "seatmap-template.html")))

;;; (HTML) File inclusion
;;; This is useful stuff! But makes for lazy programming.

(defun include-p (line)
  "Is line a well-formed include directive?"
  (let ((l (string-trim '(#\Space #\t) line)))
    (and (eql 0 (search *include-directive-start* l))
	 (search *include-directive-end* l))))

(defun get-include-fname (line)
  "Return name of file to be included."
  (let ((first-quote (search "\"" line))
	(last-quote (search "\"" line :from-end t)))
    (subseq line (1+ first-quote) last-quote)))

(defun html-include (fname)
  "Return contents of file fname. Include files referenced with <!-- #include \"fname\"-->"
  (with-open-file (stream fname)
    (loop for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (if (include-p line)
	      (html-include (get-include-fname line))
	      (progn
		(princ line)
		(terpri))))))

