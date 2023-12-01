(load "../util.lisp")

(defun starting-digit (line &optional parse-words-p)
  (let* ((chr (char line 0))
         (digit (digit-char-p chr)))
    (cond
      ((or digit (not parse-words-p))  digit)
      ((and (>= (length line) 3) (string= (subseq line 0 3) "one")) 1)
      ((and (>= (length line) 3) (string= (subseq line 0 3) "two")) 2)
      ((and (>= (length line) 5) (string= (subseq line 0 5) "three")) 3)
      ((and (>= (length line) 4) (string= (subseq line 0 4) "four")) 4)
      ((and (>= (length line) 4) (string= (subseq line 0 4) "five")) 5)
      ((and (>= (length line) 3) (string= (subseq line 0 3) "six")) 6)
      ((and (>= (length line) 5) (string= (subseq line 0 5) "seven")) 7)
      ((and (>= (length line) 5) (string= (subseq line 0 5) "eight")) 8)
      ((and (>= (length line) 4) (string= (subseq line 0 4) "nine")) 9)
      (t nil))))

(defun to-digits (line &optional parse-words-p)
  (let ((index 0)
        (digits(copy-list '())))
    (loop
      (let ((digit (starting-digit (subseq line index) parse-words-p)))
        (when digit (push digit digits))
        (incf index)
        (when (>= index (length line)) (return (reverse digits)))))))

(defun parse-line (line &optional parse-words-p)
  (let* ((digits (to-digits line parse-words-p))
         (tens (first digits))
         (ones (first (last digits))))
    (+ (* 10 tens) ones)))

(defun run (input &optional parse-words-p)
  (let ((digits (mapcar (lambda (line) (parse-line line parse-words-p)) input)))
    (reduce '+ digits)))

(defun part-1 (input) (run input))

(defun part-2 (input) (run input t))

(let ((sample-input-1 (read-file "sample-1.txt"))
      (sample-input-2 (read-file "sample-2.txt"))
      (input (read-file "input.txt")))
  (write (part-1 sample-input-1))(terpri) ; 142
  (write (part-1 input))(terpri)          ; 56108
  (terpri)
  (write (part-2 sample-input-2))(terpri) ; 281
  (write (part-2 input))(terpri)          ; 55652
)