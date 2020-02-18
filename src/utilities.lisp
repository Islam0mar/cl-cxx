;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CXX -*-

;;;; Basic utility functions and macros, used throughout the code.

(in-package :cxx)

(defun split-string-by (string char)
  "Returns a list of substrings of string
divided by ONE + each.
;; Note: Two consecutive pluses will be seen as
;; if there were an empty string between them."
  (declare (type string string))
  (declare (type character char))
  (remove ""
          (loop for i = 0 then (1+ j)
             as j = (position char string :start i)
             collect (subseq string i j)
             while j) :test #'equal))

(defun get-parenthes-string (string)
  "Returns a string within first (...) or nil"
  (declare (type string string))
  (if (not (position #\( string)) (return-from get-parenthes-string))
  (loop
     for i from (position #\( string) below (length string)
     for j = (position #\( string) then j
     for k = (char string i) then (char string i)
     for l = 0 then l
     do
       (case k
         (#\( (setf l (1+ l)))
         (#\) (setf l (1- l))))
       (if (= l 0)
           (return (subseq string j (1+ i))))))

(defun remove-string (rem-string full-string &key from-end (test #'eql)
                      test-not (start1 0) end1 (start2 0) end2 key)
  "returns full-string with rem-string removed"
  (let ((subst-point (search rem-string full-string
                             :from-end from-end
                             :test test
                             :start1 start1 :end1 end1
                             :start2 start2 :end2 end2 :key key)))
    (if subst-point
        (concatenate 'string
                     (subseq full-string 0 subst-point)
                     (subseq full-string (+ subst-point (length rem-string))))
        full-string)))

(defun left-trim-string-to (string char)
  "Returns a string trimmed to char exclusive"
  (declare (type string string))
  (if (position char string)
      (subseq string (1+ (position char string)))
      string))

(defun required (&optional (msg "A required argument is missing.") &rest args)
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (apply #'error msg args))


