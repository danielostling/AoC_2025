;;;; day-7.lisp

;;; This is a bit messy. There are, for example, two functions that figure out
;;; starting and ending nodes, one set for DAG and one for splitter positions.

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

(defun dag-start-nodes (dag)
  "Return list of all nodes in `dag` that has no incoming edges."
  (let ((dst-nodes '()))
    ;; Build destination nodes first.
    (loop :for edges :being :the :hash-value :of dag
          :do (loop :for node :in edges
                    :do (pushnew node dst-nodes :test #'equal)))
    ;; Check for nodes that are not destinations.
    (loop :for src-node :being :the :hash-key :of dag
          :when (not (member src-node dst-nodes :test #'equal))
            :collect src-node)))

(defun dag-end-nodes (dag)
  "Return list of all nodes in `dag` that has no outgoing edges."
  (loop :for maybe-end-node :being :the :hash-key
          :using (hash-value edges) :of dag
        :when (null edges)
          :collect maybe-end-node))

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
  "Return T if there is a `char` at (`row` `col`) in `arr`, else NIL.
  If position is outside of `arr` bounds, return NIL."
  (destructuring-bind (rows cols) (array-dimensions arr)
    (cond ((or (minusp row)
               (minusp col)
               (>= row rows)
               (>= col cols))
           nil)
          (t (char= (aref arr row col) char)))))

(defun splitter-p (arr row col)
  "Return T if there is a splitter at (`row` `col`) in `arr`, else NIL."
  (compare-p arr row col #\^))

(defun beam-p (arr row col)
  (compare-p arr row col #\|))

(defun free-p (arr row col)
  (compare-p arr row col #\.))

(defun get-end-node (splitter-positions)
  "Return node with largest row value."
  (let ((end-node '(0 -1)))
    (loop :for (col row) :in splitter-positions
          :when (> row (second end-node))
            :do (setf end-node `(,col ,row)))
    end-node))

(defun next-node (splitter-positions col row)
  "Starting from (`col` `row`), return the splitter the beam will hit in
  `splitter-positions`. If beam will pass last row, simulate hit to end node.
  If (`col` `row`) is the end node, return NIL."
  (let ((end-node (get-end-node splitter-positions))
        (candidate-nodes
          ;; splitter-positions are sorted ascending both for rows and cols.
          (loop :for (maybe-col maybe-row) :in splitter-positions
                :when (and (= col maybe-col)
                           (> maybe-row row))
                  :collect `(,maybe-col ,maybe-row))))
    (cond ((null candidate-nodes) end-node)
          ((equal `(,col ,row) end-node) nil)
          (t (first candidate-nodes)))))

(defun mark (input col row)
  (let ((output (shallow-copy-array input)))
    (setf (aref output row col) #\X)
    (format t "~a~%" output)))

(defun source-nodes (graph dst-node)
  "Return list of nodes/keys in `graph` that has value containing `dst-node`."
  (loop :for src-node :being :the :hash-key
          :using (hash-value edges) :of graph
        :when (member dst-node edges :test #'equal)
          :collect src-node))

(defun make-beam-graph (input starting-row starting-col)
  "Construct full beam graph from `input` and return (start-pos end-pos graph)."
  ;; Positions in the graph are (col row), *NOT* (row col).
  (let* ((rows (array-dimension input 0))
         (cols (array-dimension input 1))
         (splitter-positions '())
         (beam-graph (make-hash-table :test #'equal)))
    ;; Insert start and (made-up) end node.
    (setf (gethash `(,starting-col ,starting-row) beam-graph) '()
          (gethash `(,starting-col ,rows) beam-graph) '())
    (push `(,starting-col ,rows) splitter-positions)
    ;; Insert all beam splitters as nodes.
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :when (char= (aref input row col) #\^)
                      :do (progn
                            (setf (gethash `(,col ,row) beam-graph) '())
                            (push `(,col ,row) splitter-positions))))
    ;; Sort nodes by row and then col.
    (setf splitter-positions
          (stable-sort splitter-positions
                       (lambda (node-a node-b)
                         (or (< (second node-a) (second node-b))
                             (and (= (second node-a) (second node-b))
                                  (< (first node-a) (first node-b)))))))
    ;; Add edge from starter position to first splitter manually
    (push (next-node splitter-positions starting-col starting-row)
          (gethash `(,starting-col ,starting-row) beam-graph))
    ;; Loop across all nodes, add edges.
    (loop :for (col row) :in splitter-positions
          :for left-of-node = (free-p input row (1- col))
          :for right-of-node = (free-p input row (1+ col))
          :when left-of-node
            :do (let ((dst-node (next-node splitter-positions (1- col) row)))
                  (unless (null dst-node)
                    (push dst-node (gethash `(,col ,row) beam-graph))))
          :when right-of-node
            :do (let ((dst-node (next-node splitter-positions (1+ col) row)))
                  (unless (null dst-node)
                    (push dst-node (gethash `(,col ,row) beam-graph))))
          )
    ;; Reset all edges for nodes that has no incoming edge.
    (loop :for node :being :the :hash-key
            :using (hash-value edges) :of beam-graph
          :when (and (not (equal node `(,starting-col ,starting-row)))
                     (null (source-nodes beam-graph node)))
            :do (remhash node beam-graph))
    beam-graph))

(defun count-paths (beam-graph start-node end-node)
  "Count number of paths in `beam-graph` from `start-node` to `end-node`."
  ;; Use a memoizing DFS for this.
  (let ((memo (make-hash-table :test #'equal)))
    (labels ((dfs-traverse (node)
               (cond
                 ((equal node end-node) 1)
                 ((gethash node memo))
                 (t (setf (gethash node memo)
                          (reduce #'+
                                  (mapcar #'dfs-traverse
                                          (gethash node beam-graph))
                                  :initial-value 0))))))
      (dfs-traverse start-node))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((start-col (starting-column input))
         (beam-graph (make-beam-graph input 0 start-col))
         (splitters-hit (loop :for node :being :the :hash-key :of beam-graph
                              :collect node)))
    ;; Remove starting and ending artificial nodes.
    (- (length splitters-hit) 2)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let* ((start-col (starting-column input))
         (beam-graph (make-beam-graph input 0 start-col))
         (start-node (dag-start-nodes beam-graph))
         (end-node (dag-end-nodes beam-graph)))
    (count-paths beam-graph
                 (first start-node)
                 (first end-node))))

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
