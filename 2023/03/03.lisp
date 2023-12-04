(load "../util.lisp")

(defun symbol-p (chr)
  (and (not (char= chr #\.)) (not (digit-char-p chr)))
)

(defun get-part-number (schematic r c)
  (block get-part-number
      (let* ((line (nth r schematic))
             (num (and (digit-char-p (char line c)) (parse-integer (subseq line c) :junk-allowed t)))
             (num-length (length (write-to-string num))))
        (when (not num) (return-from get-part-number nil))
        (when (and (> c 0) (digit-char-p (char line (1- c))) (return-from get-part-number nil)))
        (dotimes (dr 3) (dotimes (dc (+ num-length 2))
          (let ((r2 (+ (1- r) dr)) (c2 (+ (1- c) dc)))
            (when (and (>= r2 0) (< r2 (length schematic)) (>= c2 0) (< c2 (length line)))
              (when (symbol-p (char (nth r2 schematic) c2)) (return-from get-part-number num)))))))
    nil))

(defun get-number-containing (schematic r c)
  (block get-number-containing
    (when (not (digit-char-p (char (nth r schematic) c))) (return-from get-number-containing nil))
    (let ((row (nth r schematic))
          (num-start c))
      (loop
        (when (or (<= num-start 0) (not (digit-char-p (char row (1- num-start))))) (return))
        (decf num-start))
      (list (parse-integer (subseq row num-start) :junk-allowed t) r num-start))))

(defun part-1 (input)
  (let ((rows (length input)) (cols (length (first input)))(sum 0))
    (dotimes (r rows) (dotimes (c cols)
        (let ((part-number (get-part-number input r c))) (when part-number (incf sum part-number)))))
    sum))

(defun part-2 (input)
  (let ((rows (length input))
        (cols (length (first input)))
        (sum 0))
    (dotimes (r rows) (dotimes (c cols)
      (when (char= #\* (char (nth r input) c))
        (let ((part-nums (copy-list '())))
          (dolist (dr '(-1 0 1)) (dolist (dc '(-1 0 1))
            (let ((r2 (+ r dr)) (c2 (+ c dc)))
              (when (and (>= r2 0) (< r2 (length input)) (>= c2 0) (< c2 (length (first input))))
                (let ((part-number (get-number-containing input r2 c2)))
                (when (and part-number (not (find-if (lambda (x) (and (= (second x) (second part-number)) (= (third x) (third part-number)))) part-nums))) (push part-number part-nums)))))))
          (when (= 2 (length part-nums)) (incf sum (* (first (first part-nums)) (first (second part-nums)))))))))
    sum))

(let ((sample-input (read-file "sample.txt"))
      (input (read-file "input.txt")))
  (write (part-1 sample-input))(terpri) ; 4361
  (write (part-1 input))(terpri)        ; 519444
  (terpri)
  (write (part-2 sample-input))(terpri) ; 467835
  (write (part-2 input))(terpri)        ; 74528807
)
