(load "../util.lisp")

(defun read-and-parse (filename)
  (let* ((lines (read-file filename)))
    (mapcar (lambda (line)
      (mapcar (lambda (row) (mapcar 'parse-integer (remove-if (lambda (x) (string= x "")) (split-string row))))
        (split-string (second (split-string line #\:)) #\|))) lines)))

(defun get-match-count (winning have)
  (reduce (lambda (acc cur) (if (find cur have) (1+ acc) acc)) winning :initial-value 0))

(defun part-1 (table)
  (let ((sum 0))
    (dolist (card table)
        (let* ((match-count (apply 'get-match-count card)))
          (when (> match-count 0) (incf sum (expt 2 (1- match-count))))))
      sum))

(defun part-2 (table)
  (let ((table-with-counts (mapcar (lambda (row) (list 1 row)) table))
        (idx 0))
    (dolist (row table-with-counts)
      (let* ((count (first row))
             (card (second row))
             (match-count (apply 'get-match-count card)))
        (dotimes (i match-count)
          (incf (first (nth (+ idx i 1) table-with-counts)) count))
      (incf idx)))
    (reduce '+ (mapcar 'first table-with-counts))))

(let ((sample-input (read-and-parse "sample.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input))(terpri) ; 13
  (write (part-1 input))(terpri)        ; 21105
  (terpri)
  (write (part-2 sample-input))(terpri) ; 30
  (write (part-2 input))(terpri)        ; 5329815
)