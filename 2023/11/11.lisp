(load "../util.lisp")

(defun read-and-parse (filename)
  (let* ((input (read-file filename))
         (galaxies (copy-list '())))
    (dotimes (r (length input)) (dotimes (c (length (nth r input)))
      (let ((char (char (nth r input) c))) (when (char= char #\#) (push `(,r ,c) galaxies)))))
    galaxies))

(defun expand-row (original-galaxies new-galaxies r count)
  (dotimes (i (length original-galaxies))
    (let ((original-galaxy (nth i original-galaxies))
          (new-galaxy (nth i new-galaxies)))
      (when (> (first original-galaxy) r) (incf (first new-galaxy) (1- count))))))

(defun expand-col (original-galaxies new-galaxies c count)
  (dotimes (i (length original-galaxies))
    (let ((original-galaxy (nth i original-galaxies))
          (new-galaxy (nth i new-galaxies)))
      (when (> (second original-galaxy) c) (incf (second new-galaxy) (1- count))))))

(defun deep-copy (galaxies)
  (let ((new-galaxies (copy-list '())))
    (dolist (g galaxies) (push (copy-list g) new-galaxies))
    (reverse new-galaxies)))

(defun expand (galaxies count)
  (let ((new-galaxies (deep-copy galaxies))
        (max-r (1+ (apply 'max (mapcar 'first galaxies))))
        (max-c (1+ (apply 'max (mapcar 'second galaxies)))))
    (dotimes (r max-r)
      (let ((empty t))
        (dotimes (c max-c)
          (when (find-if (lambda (x) (equal x `(,r ,c))) galaxies) (setf empty nil) (return)))
        (when empty (expand-row galaxies new-galaxies r count))))
    (dotimes (c max-c)
      (let ((empty t))
      (dotimes (r max-r)
        (when (find-if (lambda (x) (equal x `(,r ,c))) galaxies) (setf empty nil)))
      (when empty (expand-col galaxies new-galaxies c count))))
    new-galaxies))

(defun dist (a b)
  (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))

(defun get-sum (input expand-count)
  (let* ((expanded (expand input expand-count))
         (sum 0))
    (dotimes (i (1- (length expanded)))
      (dotimes (j (- (length expanded) i 1))
        (incf sum (dist (nth i expanded) (nth (+ i j 1) expanded)))))
    sum))

(defun part-1 (input) (get-sum input 2))

(defun part-2 (input) (get-sum input 1000000))

(let ((sample-input (read-and-parse "sample.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input))(terpri)      ; 374
  (write (part-1 input))(terpri)             ; 10494813
  (terpri)
  (write (get-sum sample-input 10))(terpri)  ; 1030 
  (write (get-sum sample-input 100))(terpri) ; 8410
  (write (part-2 input))(terpri)             ; 840988812853
  (terpri)
)