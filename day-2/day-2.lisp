;;;; day-2.lisp

(in-package #:day-2)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
  Input is a single line with ranges separated by commas.
  Convert, for example, the input
  11-22,95-115,998-1012
  into
  ((11 22) (95 115) (998 1012))"
  (loop :for (start end)
          :on (mapcar #'parse-integer
               (uiop:split-string (first lines) :separator '(#\, #\-)))
        :by #'cddr
        :collect `(,start ,end)))

(defun numbers-base-10 (n)
  "Return how many digits there are in a base 10 integer."
  (cond ((zerop n) 1)
        (t (+ 1 (floor (log (abs n) 10))))))

(defun all-equal (list &key (test #'equal))
  "Return T if all elements in `lst` are test-equal, else NIL."
  (cond ((null list) t)
        ((= (length list) 1) nil)
        (t (let ((first-element (car list)))
             (every (lambda (element)
                      (funcall test element first-element))
                    (cdr list))))))

(defun check-product-id-part-1 (product-id digits)
  "Return `product-id` if it's a bad one, else 0.
  Bad is defined as first half of number being equal to second part of number if number is split in
  half."
  (let* ((half-digits (floor digits 2))
         (pair (multiple-value-list (floor product-id (expt 10 half-digits)))))
    (if (= (first pair) (second pair))
        product-id
        0)))

(defun split-and-check (number group-size number-length)
  "Split integer `n` into list of equal length integers of length `group-size`.
   If all numbers in list are equal, return the original `number`, else 0. "
  (if (zerop number)
      0
      (let* ((tmp-number number)
             (exponent group-size)
             (divisor (expt 10 exponent))
             (splits (if (= (mod number-length group-size) 0)
                         (let ((groups ()))
                           (loop
                             :while (plusp tmp-number)
                             :for group-value = (mod tmp-number divisor)
                             :do (setf groups (cons (mod tmp-number divisor) groups)
                                       tmp-number (floor tmp-number divisor))
                             :finally (return groups)))
                         '(0))))
        (cond ((equal splits '(0)) 0)
              ((all-equal splits :test #'=) number)
              (t 0)))))

(defun check-product-id-part-2 (product-id digits)
  "Return `product-id` if it's a bad one, else 0.
  Bad is defined ikn problem statement as
  '''
    An ID is invalid if it is made only of some sequence of digits repeated at least twice. So,
    12341234 (1234 two times), 123123123 (123 three times), 1212121212 (12 five times), and
    1111111 (1 seven times) are all invalid IDs.
  '''"
  (let* ((max-group-size (if (evenp digits)
                             (floor digits 2)
                             (ceiling digits 3)))
         (bad-product-id (loop :for group-size :from 1 :upto max-group-size
                               :for result = (split-and-check product-id group-size digits)
                               :when (plusp result)
                                 :count 1)))
    
    (if (plusp bad-product-id)
        product-id
        0)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for (range-start range-end) :in input
        :summing (loop :for cur-product-id :from range-start :upto range-end
                   :for digits = (numbers-base-10 cur-product-id)
                   :when (evenp digits)
                     :summing (check-product-id-part-1 cur-product-id digits))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for (range-start range-end) :in input
        :summing (loop :for cur-product-id :from range-start :upto range-end
                       :for digits = (numbers-base-10 cur-product-id)
                         :summing (check-product-id-part-2 cur-product-id digits))))

(defun main (&optional (mode :full))
  "AoC 2025 day 2 solution.
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
