(load "../util.lisp")

(defclass hand ()
  ((cards :accessor cards :initarg :cards)
   (bid :accessor bid :initarg :bid)
   (hand-type :accessor hand-type)))

(defun get-counts (cards)
  (let ((sorted (sort (copy-seq cards) #'char<))
        (counts (copy-list '()))
        (current-count '(#\Space 0)))
    (dolist (c (coerce sorted 'list))
      (when (not (char= c (first current-count))) (setf current-count (list c 0)) (push current-count counts))
      (incf (second current-count)))
    counts))

(defun get-type (cards &optional j-wild)
  (let*  ((counts (get-counts cards))
          (j-count (or (second (find-if (lambda (x) (char= (first x) #\J)) counts)) 0))
          (sorted (sort (if j-wild (remove-if (lambda (x) (char= (first x) #\J)) counts) counts) (lambda (a b) (> (second a) (second b))))))
    (cond
      ((= j-count 5) 6)
      ((= (second (first sorted)) 5) 6)
      ((and j-wild (= (+ (second (first sorted)) j-count) 5)) 6)
      ((= (second (first sorted)) 4) 5)
      ((and j-wild (= (+ (second (first sorted)) j-count) 4)) 5)
      ((and (= (second (first sorted)) 3) (= (second (second sorted)) 2)) 4)
      ((and j-wild (and (= (second (first sorted)) 2) (= (second (second sorted)) 2) (= j-count 1))) 4)  
      ((= (second (first sorted)) 3) 3)
      ((and j-wild (= (+ (second (first sorted)) j-count) 3)) 3)
      ((and (= (second (first sorted)) 2) (= (second (second sorted)) 2)) 2)
      ((= (second (first sorted)) 2) 1)
      ((and j-wild (= (+ (second (first sorted)) j-count) 2)) 1)
      (t 0))))

(defun read-and-parse (filename)
  (let ((input (read-file filename)))
    (mapcar (lambda (line)
      (let ((parts (split-string line)))
        (make-instance 'hand :cards (first parts) :bid (parse-integer (second parts)))))
      input)))

(defun card-to-int (c j-wild)
  (or (digit-char-p c)
    (case c
      (#\T 10)
      (#\J (if j-wild 0 11))
      (#\Q 12)
      (#\K 13)
      (#\A 14))))

(defun card< (a b j-wild) (< (card-to-int a j-wild) (card-to-int b j-wild)))

(defun compare-hands (a b &optional j-wild)
  (block compare-hands
    (when (not (= (hand-type a) (hand-type b)))
      (return-from compare-hands (< (hand-type a) (hand-type b))))
  (dotimes (i 5)
    (let ((ai (char (cards a) i))
          (bi (char (cards b) i)))
    (when (not (char= ai bi)) (return-from compare-hands (card< ai bi j-wild)))))))

(defun run (hands &optional j-wild)
  (dolist (h hands) (setf (hand-type h) (get-type (cards h) j-wild)))
  (let ((sorted (sort (copy-seq hands) (lambda (a b) (compare-hands a b j-wild))))
        (sum 0))
    (dotimes (i (length sorted)) (incf sum (* (1+ i) (bid (nth i sorted)))))
    sum))

(defun part-1 (hands) (run hands))

(defun part-2 (hands) (run hands t))

(let ((sample-input (read-and-parse "sample.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input))(terpri) ; 6440
  (write (part-1 input))(terpri)        ; 254024898
  (terpri)
  (write (part-2 sample-input))(terpri) ; 5905
  (write (part-2 input))(terpri)        ; 254115617
)