;;;; day-5.lisp

(in-package #:day-5)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
  Input is in two parts. First part is a number of ranges on the form
  first-last where first and last are integers.

  Next there is a blank line, separating the parts.

  Second part is a list of lines where each line is an integer. The integers
  represent ingredient IDs.

  Return a list with two elements (ranges ingredients) where ranges is a list of
  lists, where each element is a (first last) tuple of first and last range
  values, inclusive, and ingrediants is a list of integer IDs."
  (let ((ranges ())
        (ingredient-ids ()))
    (loop :for line :in lines
          :do (let ((parts (uiop:split-string line :separator '(#\-))))
                (cond 
                  ((= (length parts) 2)
                   (push `(,(parse-integer (first parts))
                           ,(parse-integer (second parts)))
                         ranges))
                  ((= (length parts) 1)
                   (push (parse-integer (first parts)) ingredient-ids)))))
    `(,ranges ,ingredient-ids)))

(defun ingredient-in-range (ingredient-id ranges)
  "Return T if `ingredient-id` is in any of the `ranges`, else NIL."
  (loop :for (start stop) :in ranges
          :thereis (and (>= ingredient-id start)
                        (<= ingredient-id stop))))

(defun merge-ranges (ranges)
  "Merge ranges such that a new set of ranges is produced which takes into
  account any overlap of the ranges.

  Case 1, no overlap:
  |----|
           |----|

  Case 2, adjacent:
  |-----|
         |----|

  Case 3, partial overlap, left:
  |----|
     |----|

  Case 4, complete overlap:
  |-------|
     |---|"
  (let* ((sorted-ranges
           (stable-sort ranges #'(lambda (a b)
                                   (cond ((> (first b) (first a)) t)
                                         ((= (first b) (first a))
                                          (> (second b) (second a)))
                                         (t nil)))))
         (merged-ranges (list (first sorted-ranges))))
    (loop :for (cur-start cur-stop) :in (rest sorted-ranges)
          :do (let ((prev-stop (cadar merged-ranges)))                
                (cond
                  ;; Case 4 - skip range entirely, no action.
                  ;; Case 1
                  ((> cur-start prev-stop)
                   (push `(,cur-start ,cur-stop) merged-ranges))

                  ;; Case 2 and case 3
                  ((and (or
                         (= (1+ prev-stop) cur-start)
                         (>= prev-stop cur-start))
                        (> cur-stop prev-stop))
                   (setf (cadar merged-ranges) cur-stop)))))  ;; Set new end
    merged-ranges))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let ((ranges (first input))
        (ingredient-ids (second input)))
    (loop :for ingredient-id :in ingredient-ids
          :count (ingredient-in-range ingredient-id ranges))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for (start stop) :in (merge-ranges (car input))
          :sum (1+ (- stop start))))

(defun main (&optional (mode :full))
  "AoC 2025 day 5 solution.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (input (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 input))
    (format t "Part 2: ~a~%" (solve-part-2 input))))
