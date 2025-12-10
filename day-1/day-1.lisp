;;;; day-1.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
  Lines is a list of left or right moves on format [L|R]steps, for example L4 or
  R40. L means increase and R means decrease. Convert (L4 R40) to (4 -40)."
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
  (flet ((do-move (start-idx steps length)
           "Move `start-idx` `steps` clicks. Return (cur-idx wraps)"
           (let ((cur-idx start-idx)
                 (wraps (floor (abs steps) length))
                 (direction (if (minusp steps) -1 1)))
             ;; When a wrap is done, `cur-idx` remains the same value.
             (decf steps (* wraps length direction))
             ;; Next, move remaining steps.
             (incf cur-idx steps)
             ;; Check final and starting position, adjust.
             (cond ((zerop cur-idx)
                    `(0 ,(1+ wraps)))
                   ((minusp cur-idx)
                    (if (zerop start-idx)
                        `(,(+ cur-idx length) ,wraps)
                        `(,(+ cur-idx length) ,(1+ wraps))))
                   ((>= cur-idx length)
                    (if (zerop start-idx)
                        `(,(- cur-idx length) ,wraps)
                        `(,(- cur-idx length) ,(1+ wraps))))
                   (t `(,cur-idx ,wraps))))))
    (let* ((dial-start 50)
           (dial-size 100)
           (cur-idx dial-start)
           (total-wraps 0))
      (loop :for move :in input
            :for (this-cur-idx this-wraps) = (do-move cur-idx move dial-size)
            :do (progn
                  (setf cur-idx this-cur-idx)
                  (incf total-wraps this-wraps)))
      total-wraps)))

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
