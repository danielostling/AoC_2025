;;;; day-4.lisp

(in-package #:day-4)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
  Lines are strings of . and @ characters. Convert these into an array where .
  is represented by a 0 and @ is represented by a 1."
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (grid (make-array `(,rows ,cols)
                           :element-type 'integer :initial-element 0)))
    (loop :for row :from 0 :below rows
          :for line :in lines
          :collect (loop :for floor-or-roll :across line
                         :for col :from 0 :below cols
                         :when (char-equal floor-or-roll #\@)
                           :do (setf (aref grid row col) 1)))
    grid))

(defun count-rolls (grid row col)
  "Given `grid`, `row` and `col`, return number of rolls surrounding that
  position."
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (rolls 0))
    (loop :for cur-row :from (1- row) :to (1+ row)
          :do (loop :for cur-col :from (1- col) :to (1+ col)
                    :do (if (and (>= cur-row 0)
                                   (< cur-row rows)
                                   (>= cur-col 0)
                                   (< cur-col cols)
                                   (not (and (= cur-row row)
                                             (= cur-col col))))
                            (incf rolls (aref grid cur-row cur-col)))))
    rolls))

(defun get-movable-coords (grid)
  "Return a list of (row col) pairs of movable rolls in `grid`."
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (rolls-movable '()))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :when (plusp (aref grid row col))
                      :do (when (> 4 (count-rolls grid row col))
                            (push `(,row ,col) rolls-movable))))
    rolls-movable))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (length (get-movable-coords input)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let ((moved-total 0))
    (loop
      :for movable-this-turn = (get-movable-coords input)
      :do (progn
            (incf moved-total (length movable-this-turn))
            (loop :for (mark-row mark-col) :in movable-this-turn
                  :when (not (null mark-row))
                    :do (setf (aref input mark-row mark-col) 0)))
      :until (= (length movable-this-turn) 0))
    moved-total))

(defun main (&optional (mode :full))
  "AoC 2025 day 4 solution.
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
