;;;; spam-filter.lisp

(in-package #:spam-filter)

;;; "spam-filter" goes here. Hacks and glory await!
;;taken from practical common lisp

(logan-pathnames:test)

(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)


(defun classification (score)
  (values
    (cond
      ((<= score *max-ham-score*) 'ham)
      ((>= score *min-spam-score*) 'spam)
      (t 'unsure))
    score))

(defclass word-feature ()
  ((word
     :initarg :word
     :accessor word
     :initform (error "must supply word")
     :documentation "the word this feature represents")
   (spam-count
     :initarg :spam-count
     :accessor spam-count
     :initform 0
     :documentation "num of spams we have seen this word in")
   (ham-count 
     :initarg :ham-count 
     :accessor ham-count 
     :initform 0 
     :documentation "num of hams we have seen this word in")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf
    *feature-database* (make-hash-table :test #'equal)
    *total-spams* 0
    *total-hams* 0)) 

(defun extract-words (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
    :test #'string=))

(defun intern-feature (word)
  (or
    (gethash word *feature-database*)
    (setf (gethash word *feature-database*)
          (make-instance 'word-feature :word word))))


(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))



(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count ) feature
    (let ((spam-frequency (/ (max 1 *total-spams*) ))
          (ham-frequency (/ (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency))))) 

(defun bayesian-spam-probability (feature &optional
                                          (assumed-probability 1/2)
                                          (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature ) (ham-count feature))))
    (/ (+ (* weight assumed-probability )
          (* data-points basic-probability))
       (+ weight data-points)))) 

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The fisher computation described by robinson"
  (inverse-chi-square
    (* -2 (reduce #'+ probs :key #'log))
    (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
    (loop with m = (/ value 2)
          for i below (/ degrees-of-freedom 2)
          for prob  = (exp (- m )) then  (* prob (/ m i )) 
          summing prob)
    1.0))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus) 
  (dolist (filename (logan-pathnames:list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
          (size (length corpus))
          (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on) 
    (test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0 ) end)
  (loop for idx from start below (or end (length corpus)) do 
        (destructuring-bind (file type)
          (aref corpus idx)
          (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
            (classify (start-of-file file *max-chars* ))
            (list
              :file file
              :type type
              :classification classification
              :score score)))))

(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
    (if (< read length)
      (subseq text 0 read)
      text))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham 
        (ecase classification 
          (ham 'correct)
          (spam 'false-positive)
          (unsure 'missed-ham)))
      (spam
        (ecase classification 
          (ham 'false-negative)
          (spam 'correct)
          (unsure 'missed-spam))))))


