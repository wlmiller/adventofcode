(load "../util.lisp")

(defun read-and-parse (filename)
  (let* ((input (read-file filename))
        (directions (first input))
        (p2-start-nodes (copy-list '()))
        (tree (make-hash-table :test #'equal)))
    (dolist (line (subseq input 2))
      (let* ((split (split-string line #\=))
             (from (remove #\Space (first split)))
             (to (split-string (remove #\Space (remove #\( (remove #\) (second split)))) #\,)))
        (setf (gethash from tree) to)
        (when (char= (char from 2) #\A) (push from p2-start-nodes))))
    (list directions tree p2-start-nodes)))

(defun move (node tree dir)
  (let ((leaves (gethash node tree)))
  (case dir
    (#\L (first leaves))
    (#\R (second leaves)))))

(defun part-1 (input)
  (let ((directions (first input))
        (tree (second input))
        (steps 0)
        (node "AAA"))
    (loop
      (let ((dir (char directions (mod steps (length directions)))))
        (setf node (move node tree dir))
        (incf steps)
        (when (equal node "ZZZ") (return))))
    steps))

; takes forever - definitely not the "right" solution
; find loops and figure out when they line up?
(defun part-2 (input)
  (let ((directions (first input))
        (tree (second input))
        (steps 0)
        (nodes (third input)))
    (loop
      (let ((dir (char directions (mod steps (length directions)))))
        (setf nodes (mapcar (lambda (node) (move node tree dir)) nodes))
        (incf steps)
        (when (every (lambda (n) (char= (char n 2) #\Z)) nodes) (return))))
    steps))

(let ((sample-input-1 (read-and-parse "sample-1.txt"))
      (sample-input-2 (read-and-parse "sample-2.txt"))
      (sample-input-3 (read-and-parse "sample-3.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input-1))(terpri) ; 2
  (write (part-1 sample-input-2))(terpri) ; 6
  (write (part-1 input))(terpri)          ; 19637
  (terpri)
  (write (part-2 sample-input-3))(terpri) ; 6
  (write (part-2 input))(terpri)
)