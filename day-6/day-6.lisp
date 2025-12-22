;;;; day-6.lisp

(in-package #:day-6)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

  Input is a set of numbers followed by a set of binary operators; + and *.  The
  numbers should be arranged column wise in as many columns as there are
  operators. White space matters, use character element type and put blanks in
  the array as well."

  ;; Step 1 is to convert 
  ;; "123 328  51 64 " 
  ;; " 45 64  387 23 " 
  ;; "  6 98  215 314"
  ;; "*   +   *   +  "
  ;;
  ;; into a vector of all the characters in the input.
  ;;
  ;; Step 2 is to convert result from step 1 into an array and return it.
  (let ((data ())
        (first-operator-idx 0))
    ;; Step 1
    (loop :for line :in lines
          :do (loop :for elt :across line
                    :do (push elt data)))
    (setf data (coerce (reverse data) 'vector))
    (setf first-operator-idx (position-if #'operator-p data))
    ;; Step 2
    (let* ((cols (- (length data) first-operator-idx))
           (rows (floor (length data) cols)))
      (make-array `(,rows ,cols) :displaced-to data))))

(defun operator-p (elt)
  "Return T if `elt` is one of chars + or *, else NIL."
  (cond ((char= elt #\+) t)
        ((char= elt #\*) t)
        (t nil)))

(defun whitespace-p (elt)
  "Return T if `elt` is a space, else NIL."
  (char= elt #\ ))

(defun make-col-stripe (arr first-col last-col)
  "Return a new array based on `arr` columns from `first-col` to `last-col`,
  inclusive. Include all rows. `first-col` has to be smaller or equal to
  `last-col`."
  (let* ((rows (array-dimension arr 0))
         (stripe-width (1+ (- last-col first-col)))
         (stripe (make-array `(,rows ,stripe-width))))
    (loop :for src-col :from first-col :to last-col
          :for dst-col = 0 :then (1+ dst-col)
          :do (loop :for row :from 0 :below rows
                    :do (setf (aref stripe row dst-col)
                              (aref arr row src-col))))
    stripe))

(defun find-separator-cols (arr)
  "Return list of column integer indices of `arr` where all rows in the col is
   whitespace."
  (flet ((col-whitespace-p (arr col)
         "Return T if `col` of `arr` only contains a whitespace character,
         else NIL."
         (let ((col-data (make-col-stripe arr col col)))
           (every #'whitespace-p
                  (loop :for idx :from 0 :below (array-total-size col-data)
                        :collect (row-major-aref col-data idx))))))
    (let ((cols (array-dimension arr 1)))
      (loop :for col :from 0 :below cols
            :when (col-whitespace-p arr col)
              :collect col))))

(defun make-equation-blocks (arr)
  "Split `arr` into a list of arrays, each representing equation blocks."
  (let* ((col-count (array-dimension arr 1))
         (separator-cols (find-separator-cols arr))
         (first-last '()))
    (let ((prev-col 0))
      (loop :for sep-col :in separator-cols
            :do (progn
                  (push `(,prev-col ,(1- sep-col)) first-last)
                  (setf prev-col (1+ sep-col))))
      (push `(,prev-col ,(1- col-count)) first-last)
      (setf first-last (reverse first-last)))
    (loop :for (first-col last-col) :in first-last
          :collect (make-col-stripe arr first-col last-col))))

(defun make-rows (arr)
  "Convert 2D array `arr` into a list of row vectors."
  (let ((rows (array-dimension arr 0))
        (cols (array-dimension arr 1)))
    (loop :for row :from 0 :below rows
          :collect (make-array cols
                    :displaced-to arr
                    :displaced-index-offset (* row cols)))))

(defun transpose-arr (arr)
  "Return transpose of `arr`."
  (let* ((rows (array-dimension arr 0))
         (cols (array-dimension arr 1))
         (transposed (make-array `(,cols ,rows))))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :do (setf (aref transposed col row)
                              (aref arr row col))))
    transposed))

(defun eq-block-operator (eq-block)
  "Return operator function in `eq-block`."
  (let* ((operator-row (first (last (make-rows eq-block))))
         (operator (string-trim '(#\ ) (coerce operator-row 'string))))
    (if (string= operator "*") #'* #'+)))

(defun make-part-2-arguments (eq-block)
  "Convert an equation block `eq-block` into list of integers for part 2."
  (let* ((rows (1- (array-dimension eq-block 0)))
         (cols (array-dimension eq-block 1))
         (just-arguments (make-array `(,rows ,cols))))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :do (setf (aref just-arguments row col)
                              (aref eq-block row col))))
    (let ((transposed (transpose-arr just-arguments)))
      (map 'list #'(lambda (vec)
                     (string-trim '(#\ ) (coerce vec 'string)))
           (make-rows transposed)))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for equation-vector :in (make-equation-blocks input)
        :for row-list = (map 'list
                             #'(lambda (vec)
                                 (string-trim '(#\ ) (coerce vec 'string)))
                             (make-rows equation-vector))
        :for operator = (if (string= (car (last row-list)) "+") #'+ #'*)
        :summing (apply operator (mapcar #'parse-integer (butlast row-list)))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let ((equation-blocks (make-equation-blocks input)))
    (loop :for eq-block :in equation-blocks
          :for operator = (eq-block-operator eq-block)
          :for arguments = (make-part-2-arguments eq-block)
          :summing (apply operator (mapcar #'parse-integer arguments)))))

(defun main (&optional (mode :full))
  "AoC 2025 day 6 solution.
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
