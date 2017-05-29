;;;; seatmap.lisp
;;;; snorre.svendsen@gmail.com
;;;; Created 2014-05-05
;;;; Last changed 2016-11-20

;;; TODO:
;;; - debug row setting / resetting

;;; Last change: Change global var names.

(in-package #:seatmap)

;;; Venue-oriented definition.
    
(defun get-venue-hash (venue-list)
  ;; venue-list is the list returned from read-venue-file in venue.lisp
  "Defines the hash table for a venue. Entries are symbols R(label).P(seat), and are set to number 0."
  (let ((venue-hash (make-hash-table)))
    (dolist (row-list venue-list)
      (dolist (seat-str 
		(remove-if #'(lambda (x) (equal x ".")) (getf row-list :seats)))
	(setf (gethash
	       (intern (concatenate 'string "R" (getf row-list :lbl)
			    ".P" seat-str))
	       venue-hash)
	      0)))
    venue-hash))

;;; Event-oriented definitions.

(defun set-event-hash (event-hash)
  "Sets current event."
  (setf *current-event* event-hash))

(defun set-seat (seat-sym val &key (quiet nil) (event *current-event*))
  "Set seat to value. Warns if seat-sym is not a seat. If QUIET is not set, also warns when a seat value is changed."
  (multiple-value-bind (current-val foundp) (gethash seat-sym event)
    (if (not foundp)
	(format t "Warning: Seat ~a is not defined for this venue. Discarded.~%" seat-sym)
	(progn
	  (when (and (not (eql current-val 0))
		     (not (eql current-val val))
                     (not quiet))
	    (format t "Warning: Seat ~a was ~a, now set to ~a.~%"
		    seat-sym current-val val))
	  (setf (gethash seat-sym event) val)))))

(defun get-seat (seat-sym &optional (event-hash *current-event*))
  "Returns value of seat. Warns if seat-sym is not defined."
  (multiple-value-bind (current-val foundp) (gethash seat-sym event-hash)
    (when (not foundp)
      (format t "Warning: Seat ~a is not defined for this venue.~%" seat-sym))
    current-val))

(defun print-event (&key (only-values nil) (event-hash *current-event*)
		      (venue-list *current-venue*))
  "Prints event, mapping event-hash onto venue-list.
Optionally, print only the value(s) in list ONLY-VALUES."
  (setf only-values (ensure-list only-values))
  (dolist (row-list venue-list)
    (format t "~3a:" (getf row-list :lbl))
    (dolist (seat-str (getf row-list :seats))
      (if (equal seat-str ".")
	  (format t " ")
	  (let ((seat-value (get-seat
			    (intern (concatenate 'string "R" (getf row-list :lbl)
						 ".P" seat-str)) event-hash)))
	    (if (not only-values)
		(if (eql 0 seat-value)
		    (format t ".")
		    (format t "~a" (char (write-to-string seat-value) 0)))
		(if (member seat-value only-values)
		    (format t "~a" (char (write-to-string seat-value) 0))
		    (format t "."))))))
      (format t "~%")))

(defun load-venue (venue-file)
  "Load venue file, and set current event to hash table of seats, with all seats set to 0."
  (setf *current-venue* (read-venue-file venue-file))
  (setf *current-event* (get-venue-hash *current-venue*)))

(defun parse-seat-string (seat-str)
  "Returns the row and seat of seat-str as strings."
  ;; Eg. "RR.P12" => "R" "12"
  ;;   "R5.P10-12" => "5" "10-12"
  (let ((lst (split "." seat-str)))
    (if (/= 2 (length lst))
	(format t "Not a valid seat specification: ~S" seat-str)
	(if (not (and (char-equal (char (first lst) 0) #\R)
		      (char-equal (char (second lst) 0) #\P)))
	    (format t "Not a valid seat specification: ~S" seat-str)
	    (values (subseq (first lst) 1) (subseq (second lst) 1))))))

(defun parse-range-string (s)
  "Parse a range string, eg. 'R12.P1-2'. Returns row as string, seats as integers. Single seat is returned as a range of itself, eg. R14.P1-1 ."
  (multiple-value-bind (row seat-range) (parse-seat-string s)
    (setf seat-range (split "-" seat-range))
    (if (= (length seat-range) 1) ; a single seat
	(values row
		(parse-integer (car seat-range))
		(parse-integer (car seat-range)))
	(values row 
		(parse-integer (car seat-range)) 
		(parse-integer (cadr seat-range))))))

(defun set-seats-f (seat-range value &key (quiet nil) (event *current-event*))
  "Set a range of seats in a row. Also sets single seat. Called by macro set-seats."
  (multiple-value-bind (row n1 n2) (parse-range-string (string seat-range))
    (let ((range (list-numbers-as-str n1 n2))
	  (seat-prefix (concatenate 'string "R" row ".P")))
      (dolist (iter range)
	(set-seat (intern (concatenate 'string seat-prefix iter)) 
		  value
                  :quiet quiet
		  :event event)))))

;; added 2014-06-16 - add-to-seat-val
;; duplicates set-seats-f in an ugly manner
(defun add-to-seat-val (seat-range value &key
                                           (quiet t)
                                           (event *current-event*))
  "Add value to the current seat value."
  (multiple-value-bind (row n1 n2) (parse-range-string (string seat-range))
    (let ((seat-prefix (concatenate 'string "R" row ".P")))
      (dolist (iter (list-numbers-as-str n1 n2))
	(let ((seat-sym (intern (concatenate 'string seat-prefix iter))))
	  (set-seat seat-sym 
		    (+ value (get-seat seat-sym event))
                    :quiet quiet
		    :event event))))))

;; added 2014-10-27 - macro set-seats
;; set seat values without quoting (')
;; eg. (set-seats R10.P1-10 blocked)
(defmacro set-seats (seat-range value &key (quiet nil) (event-hash *current-event*))
  "Set a range of seats in a row."
  `(set-seats-f ',seat-range ',value :quiet ,quiet :event ,event-hash))

(defun reset-all-seats (&optional (event-hash *current-event*))
  "Set all seats to 0."
  (loop for key being the hash-keys in event-hash
     do (setf (gethash key event-hash) 0)))

(defun list-seats (seat-value &optional (event-hash *current-event*))
  "Return list of seats set to value."
  (let ((result))
    (maphash #'(lambda (key val)
		 (when (equal val seat-value)
		   (push key result)))
	     event-hash)
    (nreverse result)))

(defun set-seat-list (seat-list value &key (event *current-event*))
  "Set seats in SEAT-LIST to VALUE." 
  (dolist (seat seat-list)
    (set-seats-f seat value :event event)))

(defun change-seats (from-value to-value &optional (event-hash *current-event*))
  "Change all seats with value FROM-VALUE to TO-VALUE."
  (set-seat-list (list-seats from-value event-hash) to-value :event event-hash))

(defun count-seats (seat-value &optional (event-hash *current-event*))
  "Count occurences of seat-value in event."
  (length (list-seats seat-value event-hash)))

(defun list-values (&optional (event-hash *current-event*))
  "List all seat values in event."
  (let ((result))
    (loop for val being the hash-value of event-hash do
	 (when (not (member val result))
	   (push val result)))
    (reverse result)))

(defun print-report (&optional (event-hash *current-event*))
  "Reports seat values and the number of each."
  (dolist (value (list-values event-hash))
    (format t "~A: ~A~%" value (count-seats value event-hash))))

;;; row handling - in development

(defun set-row-f (row-lbl value &optional (event-hash *current-event*))
  "Function to set a row of seats (eg. 'R10'). Called by macro set-row."
  (let ((seat-prefix (concatenate 'string "R" row-lbl ".P")))
    (dolist (iter (list-all-seats row-lbl))
      (set-seat (intern (concatenate 'string seat-prefix iter))
                  value
                  :event event-hash))))

(defun set-rows (r1 r2 value &optional (event-hash *current-event*))
  "Set all seats in rows from R1 to R2. Works only where rows are numbers."
  (dolist (row (list-numbers-as-str r1 r2))
    (set-row-f row value event-hash)))

;; Improve: row should be checked against "Rn" - where n is row label as string
(defmacro %set-row (row value &optional (event-hash *current-event*))
  "Set all seats in a row."
  `(set-row-f (mkstr ,row) (symb ',value) ,event-hash))
