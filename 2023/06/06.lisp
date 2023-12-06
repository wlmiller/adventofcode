(load "../util.lisp")

(defun parse-line-1 (line)
  (mapcar 'parse-integer (remove-if (lambda (x) (string= "" x)) (split-string (second (split-string line #\:))))))

(defun parse-line-2 (line)
  `(,(parse-integer (coerce (remove-if (lambda (x) (char= #\Space x)) (coerce (second (split-string line #\:)) 'list)) 'string))))

(defun parse (parse-fun lines)
  (let* ((times (funcall parse-fun (first lines)))
         (distances (funcall parse-fun (second lines)))
         (input (copy-list '())))
    (dotimes (i (length times)) (push `(,(nth i times) ,(nth i distances)) input))
    (reverse input)))

(defun solve-quadratic (a b c)
  (let* ((ad (coerce a 'double-float))
         (bd (coerce b 'double-float))
         (cd (coerce c 'double-float))
         (d (sqrt (- (* bd bd) (* 4 ad cd))))
         (e (* 2 ad)))
    `(,(/ (+ (- bd) d) e) ,(/ (- (- bd) d) e))))

(defun run (lines parse-line-fun)
  (let ((input (parse parse-line-fun lines)))
  (reduce '* (mapcar (lambda (x)
    (let ((range (sort (solve-quadratic 1 (- (first x)) (1+ (second x))) #'<)))
      (1+ (- (floor (second range)) (ceiling (first range))))))
    input))))

(defun part-1 (lines) (run lines 'parse-line-1))

(defun part-2 (lines) (run lines 'parse-line-2))

(let ((sample-input (read-file "sample.txt"))
      (input (read-file "input.txt")))
  (write (part-1 sample-input))(terpri) ; 288
  (write (part-1 input))(terpri)        ; 1312850
  (terpri)
  (write (part-2 sample-input))(terpri) ; 71503
  (write (part-2 input))(terpri)        ; 36749103
  (terpri)
)