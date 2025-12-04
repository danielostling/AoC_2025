;;;; day-1.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

  Lines is a list of left or right moves on format [L|R]steps, for example L4 or
  R40.

  L means increase and R means decrease. Convert (L4 R40) to (4 -40)."
  (loop :for move :in lines
        :for direction = (if (string-equal "L" (subseq move 0 1)) -1 1)
        :for steps = (* direction (parse-integer (subseq move 1)))
        :collect steps))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (flet ((do-move (steps cur-idx length)
           (mod (+ cur-idx steps) length)))
    (let ((dial-start 50)
          (dial-size 100))
      (loop :for move :in (cons dial-start input)
            :for cur-pos = move :then (do-move move cur-pos dial-size)
            :count (zerop cur-pos)))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (flet ((do-move (steps cur-idx length)
           
           
           (mod (+ cur-idx steps) length)))
    (let ((dial-start 50)
          (dial-size 100))
      (loop :for move :in (cons dial-start input)
            :for cur-pos = move :then (do-move move cur-pos dial-size)
            :count (zerop cur-pos))))
)

(defun main (&optional (mode :full))
  "AoC 2025 day 1 solution.
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
