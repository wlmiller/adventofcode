(load "../util.lisp")

(defun read-and-parse (filename)
  (let ((input (read-file filename)))
    (mapcar (lambda (line) (mapcar 'parse-integer (split-string line))) input)))

(defun extend (list)
  (let* ((seq (reverse list))
         (seqs `(,seq)))
    (loop
      (let ((new-seq (copy-list '())))
        (when (every (lambda (x) (= x 0)) seq) (return))
        (setf new-seq (copy-list '()))
        (dotimes (i (1- (length seq))) (push (- (nth i seq) (nth (1+ i) seq)) new-seq))
        (push (reverse new-seq) seqs)
        (setf seq (reverse new-seq))))
    (dotimes (i (1- (length seqs)))
      (let ((prev-seq (nth i seqs))
            (seq (nth (1+ i) seqs)))
        (push (+ (first seq) (first prev-seq)) seq)
        (setf (nth (1+ i) seqs) seq)))
    (caar (last seqs))))

(defun part-1 (input) (reduce #'+ (mapcar 'extend input)))

(defun part-2 (input) (reduce #'+ (mapcar (lambda (list) (extend (reverse list))) input)))

(let ((sample-input (read-and-parse "sample.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input))(terpri) ; 114
  (write (part-1 input))(terpri)        ; 1868368343
  (terpri)
  (write (part-2 sample-input))(terpri) ; 2
  (write (part-2 input))(terpri)        ; 1022
)