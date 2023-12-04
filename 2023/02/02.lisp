(load "../util.lisp")

(defun get-color-and-count (str)
  (let ((vals (remove-if (lambda (x) (string= x "")) (split-string str))))
    `(,(parse-integer (first vals)) ,(intern (string-upcase (second vals)) :keyword))))

(defun read-and-parse (filename)
  (mapcar (lambda (line)
    (mapcar (lambda (game)
      (first (mapcar (lambda (subset)
        (mapcar 'get-color-and-count (split-string subset #\,)))
      (split-string game #\;))))
    (split-string (second (split-string line #\:)) #\;)))
    (read-file filename)))

(defun possible-p (subset)
  (block possible
    (dolist (ball-count subset)
      (let ((count (first ball-count))
            (color (second ball-count)))
        (cond 
          ((and (equal color :red) (> count 12)) (return-from possible nil))
          ((and (equal color :green) (> count 13)) (return-from possible nil))
          ((and (equal color :blue) (> count 14)) (return-from possible nil)))))
    t))

(defun get-mins (game)
  (let ((mins (copy-list '())))
    (dolist (color '(:red :green :blue))
      (let ((min-count 0))
        (dolist (subset game)
          (let ((ball-count (find-if (lambda (x) (equal (second x) color)) subset)))
            (when ball-count (setf min-count (max min-count (first ball-count))))))
        (push `(,min-count ,color) mins)))
    mins))

(defun part-1 (input)
  (let ((game-num 1)
        (sum 0))
      (dolist (game input)
        (when (reduce (lambda (acc cur) (and acc cur)) (mapcar 'possible-p game)) (incf sum game-num))
        (incf game-num)) 
      sum))

(defun part-2 (input)
  (let ((sum 0))
    (dolist (game input)
      (incf sum (reduce '* (mapcar 'first (get-mins game)))))
    sum))

(let ((sample-input (read-and-parse "sample.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input))(terpri) ; 8
  (write (part-1 input))(terpri)        ; 2268
  (terpri)
  (write (part-2 sample-input))(terpri) ; 2286
  (write (part-2 input))(terpri)        ; 63542
)