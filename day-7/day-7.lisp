;;;; day-7.lisp

(in-package #:day-7)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
  Input is a tachyon beam splitter diagram. Parse the lines into an array."
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (beam-splitter (make-array `(,rows ,cols))))
    (loop :for line :in lines
          :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :for char :across line
                    :do (setf (aref beam-splitter row col) char)))
    beam-splitter))

(defun starting-column (arr)
  "Return column of the 'S' in the first row of `arr`."
  (let ((cols (array-dimension arr 1)))
    (loop :for col :from 0 :below cols
          :when (char= (aref arr 0 col) #\S)
            :do (return col))))

(defun shallow-copy-array (arr)
  "Return a shallow copy of `arr`."
  (let ((cpy (make-array (array-dimensions arr)
                         :element-type (array-element-type arr))))
    (loop :for row :from 0 :below (array-dimension arr 0)
          :do (loop :for col :from 0 :below (array-dimension arr 1)
                    :do (setf (aref cpy row col) (aref arr row col))))
    cpy))

(defun compare-p (arr row col char)
  "Return T if there is a `char` at (`row` `col`) in `arr`, else NIL."
  (char= (aref arr row col) char))

(defun splitter-p (arr row col)
  "Return T if there is a splitter at (`row` `col`) in `arr`, else NIL."
  (compare-p arr row col #\^))

(defun beam-p (arr row col)
  (compare-p arr row col #\|))

(defun free-p (arr row col)
  (compare-p arr row col #\.))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((tachyon-splitter (shallow-copy-array input))
         (starting-col (starting-column tachyon-splitter))
         (last-row (1- (array-dimension tachyon-splitter 0)))
         (last-col (1- (array-dimension tachyon-splitter 1)))
         (beam-splits 0))
    (setf (aref tachyon-splitter 1 starting-col) #\|)

    ;; When looping across the rows, check the current position if there is a
    ;; splitter or not (^), but also check above for a beam (|).
    (loop :for row :from 2 :upto last-row
          :do (loop :for col :from 0 :upto last-col
                    :do (cond ((and (free-p tachyon-splitter row col)        ;; at ., | above
                                    (beam-p tachyon-splitter (1- row) col)) 
                               (setf (aref tachyon-splitter row col) #\|))
                              ((and (splitter-p tachyon-splitter row col)    ;; at ^, | above
                                    (beam-p tachyon-splitter (1- row) col))
                               (progn
                                 (incf beam-splits)
                                 (when (free-p tachyon-splitter row (1- col))
                                   (setf (aref tachyon-splitter row (1- col)) #\|))
                                 (when (free-p tachyon-splitter row (1+ col))
                                   (setf (aref tachyon-splitter row (1+ col)) #\|))
                                 ))
                              )
                    )
          )
    ;;(format t "~a~%~%" tachyon-splitter)

    ;; (loop :for col :from 0 :to last-col
    ;;       :count (char= (aref tachyon-splitter last-row col) #\|) )
    beam-splits
    )
  )

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  1)

(defun main (&optional (mode :full))
  "AoC 2025 day 7 solution.
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
