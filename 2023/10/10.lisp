(load "../util.lisp")

(defun read-and-parse (filename)
  (let* ((input (read-file filename))
         (output (make-array `(,(length input) ,(length (first input)))))
         (start-pos))
    (dotimes (r (length input))
      (let ((row (nth r input)))
        (dotimes (c (length row))
          (let ((char (char row c)))
            (case char
              (#\S (setf (aref output r c) :start) (setf start-pos `(,r ,c)))
              (#\. (setf (aref output r c) :ground))
              (#\| (setf (aref output r c) (list `(,(1- r) ,c) `(,(1+ r) ,c))))
              (#\- (setf (aref output r c) (list `(,r ,(1- c)) `(,r ,(1+ c)))))
              (#\L (setf (aref output r c) (list `(,(1- r) ,c) `(,r ,(1+ c)))))
              (#\J (setf (aref output r c) (list `(,(1- r) ,c) `(,r ,(1- c)))))
              (#\7 (setf (aref output r c) (list `(,r ,(1- c)) `(,(1+ r) ,c))))
              (#\F (setf (aref output r c) (list `(,r ,(1+ c)) `(,(1+ r) ,c))))))) ))
    `(,output ,start-pos ,input)))

(defun get-loop (map start-pos)
  (let (
        (prev-pos start-pos)
        (pos)
        (loop (copy-list '())))
    (dolist (dir (list '(-1 0) '(1 0) '(0 -1) '(0 1)))
      (let* ((next-pos `(,(+ (first start-pos) (first dir)) ,(+ (second start-pos) (second dir))))
             (next-val (when (and (>= (first next-pos) 0) (< (first next-pos) (array-dimension map 0)) (>= (second next-pos) 0) (< (second next-pos) (array-dimension map 1)))
              (aref map (first next-pos) (second next-pos)))))
        (when (and
          next-val
          (not (eq next-val :ground))
          (find-if (lambda (x) (equal start-pos x)) next-val)) (setf pos next-pos) (return))))
    (loop
      (let* ((val (aref map (first pos) (second pos)))
             (next-pos (find-if (lambda (x) (not (equal prev-pos x))) val)))
        (setf prev-pos pos)
        (setf pos next-pos)
        (push pos loop)
        (when (equal pos start-pos) (return))))
    loop))

(shadow 'fill)
; works for the first example, doesn't handle "squeezing" between pipes
; track along the loop and keep track of an "a" and "b" side of the pipes all the way around?
; would divide the space and then the "outside" is the one that hits the edge
(defun fill (map start loop)
  (let ((filled `(,start))
        (frontier `(,start)))
    (loop
      (let ((next-frontier (copy-list '())))
        (dolist (pos frontier)
          (dolist (dir (list '(-1 0) '(1 0) '(0 -1) '(0 1)))
            (let ((next-pos `(,(+ (first pos) (first dir)) ,(+ (second pos) (second dir)))))
              (when
                (and
                  (>= (first next-pos) 0) (< (first next-pos) (array-dimension map 0))
                  (>= (second next-pos) 0) (< (second next-pos) (array-dimension map 1))
                  (not (find-if (lambda (x) (equal x next-pos)) filled))
                  (not (find-if (lambda (x) (equal x next-pos)) loop)))
                (push next-pos next-frontier)
                (push next-pos filled)))))
        (when (not next-frontier) (return))
        (setf frontier next-frontier)))
  filled))

(defun part-1 (input)
  (let ((loop (get-loop (first input) (second input))))
    (/ (1+ (length loop)) 2)))

(defun part-2 (input)
  (let* ((map (first input))
        (loop (get-loop map (second input)))
         (outside (fill map '(0 0) loop)))
    (- (* (array-dimension map 0) (array-dimension map 1)) (length loop) (length outside))))

(let ((sample-input-1 (read-and-parse "sample-1.txt"))
      (sample-input-2 (read-and-parse "sample-2.txt"))
      (sample-input-3 (read-and-parse "sample-3.txt"))
      (sample-input-4 (read-and-parse "sample-4.txt"))
      (sample-input-5 (read-and-parse "sample-5.txt"))
      (sample-input-6 (read-and-parse "sample-6.txt"))
      (input (read-and-parse "input.txt")))
  (write (part-1 sample-input-1))(terpri) ; 4
  (write (part-1 sample-input-2))(terpri) ; 8
  (write (part-1 input))(terpri)          ; 7005
  (terpri)
  (write (part-2 sample-input-3))(terpri) ; 4
  (write (part-2 sample-input-4))(terpri) ; 4 (incorrect)
  (write (part-2 sample-input-5))(terpri) ; 8 (incorrect)
  (write (part-2 sample-input-6))(terpri) ; 10 (incorrect)
  (write (part-2 input))(terpri)
)