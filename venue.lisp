;;;; venue.lisp
;;;; snorre.svendsen@gmail.com
;;;; Created 2014-05-07
;;;; Modified 2015-08-02

;;;; Part of package "seatmap"

;;;; Build venues from a text file specification.
;;;; Example of a venue text file:
#|
;test_venue.ven
 1:   12345 67890 .234
 2: 1234567 89012 .4567
from-right
 3: 987654 321
|#
;;;; This venue consists of three rows, labeled "1" and "2".
;;;; Labels may be any string.
;;;; Any characters from ":" to the end of line are interpreted as seats numbered from 1 upwards, except:
;;;; 1. #\Space - Open space, e.g. gap in a row, leading or trailing spaces.
;;;; 2. #\. - Increase seat count without registering a seat in the venue.
;;;; Any characters may be used; use digits to ease counting.
;;;; The line "from-right" sets seat count from right to left for the following input.
;;;; Set back to counting from left with a line containing "from-left".

(in-package #:seatmap)

(defparameter *from-right* nil "Flagged if venue file includes line 'from-right'.")

(defun read-venue-line (line)
  "Returns label and list of seat numbers of the line."
  (let* ((delim-pos (position #\: line :test #'equal))
	 (lbl (string-trim " " (subseq line 0 delim-pos)))
	 (seat-lst (coerce (subseq line (1+ delim-pos)) 'list))
	 (count 0) (result))
    (when *from-right*
      (setf seat-lst (nreverse seat-lst)))
    (dolist (iter seat-lst)
      (cond
	((char-equal iter #\Space) (push "." result))
	((char-equal iter #\.) (progn
				 (incf count)
				 (push "." result)))
	(t (progn
	     (incf count)
	     (push (write-to-string count) result)))))
    (if *from-right*
	(list :lbl lbl :seats result)
	(list :lbl lbl :seats (nreverse result)))))

(defun read-venue-file (fname)
  "Returns list of row labels and seats from text file. Comments are printed."
  (let ((result))
    (setf *from-right* nil) ; default is reading from left
    (with-open-file (stream fname)
      (loop for line = (read-line stream nil 'eof)
	 until (eq line 'eof)
	 do (cond 
	      ((eql #\; (char line 0))
	       (format t "~a~%" (subseq line 1))) ; print comment
	      ((equal "FROM-RIGHT" (string-upcase line))
	       (setf *from-right* t))
	      ((equal "FROM-LEFT" (string-upcase line))
	       (setf *from-right* nil))
	      (t
	       (push (read-venue-line line) result)))))
    ;; here should be check for duplicate row labels
    (nreverse result)))

(defun print-venue (&optional (venue-list *current-venue*))
  "Prints venue with labels and seat numbers."
  (dolist (row-list venue-list)
    (format t "~3a:" (getf row-list :lbl))
    (dolist (seat-str (getf row-list :seats))
      (if (equal seat-str ".")
	  (format t " ")
	  (format t "~d" (mod (parse-integer seat-str) 10))))
    (format t "~%")))

(defun list-all-seats (row-lbl &optional (venue-list *current-venue*))
  "Returns list of all seats in a venue row. Empty seats are omitted."
  (dolist (row-list venue-list)
    (if (equal row-lbl (getf row-list :lbl))
          (return (remove-if #'(lambda (elem) (equal "." elem))
                                  (getf row-list :seats))))))

(defun list-all-rows (&optional (venue-list *current-venue*))
  "Returns list of all rows in a venue. Empty labels are omitted."
  (let ((result))
    (dolist (row-list venue-list)
      (let ((row-lbl (getf row-list :lbl)))
        (when (not (string= row-lbl ""))
          (push row-lbl result))))
    (reverse result)))

(defun get-seat-rank (seat-sym &optional (venue-list *current-venue*))
  "Return a rank of seat in venue - an arbitrary number set to row times 1000 plus seat."
  (multiple-value-bind (row-str seat-str) (parse-seat-string (string seat-sym))
    (+ (* 1000 (position row-str venue-list :key #'cadr :test #'string-equal))
       (parse-integer seat-str))))

(defun seat-less-p (seat1 seat2 &optional (venue-list *current-venue*))
  "Return T if rank of SEAT1 is less than SEAT2. Used to sort lists of seats into natural order."
  (< (get-seat-rank seat1 venue-list) (get-seat-rank seat2 venue-list)))


