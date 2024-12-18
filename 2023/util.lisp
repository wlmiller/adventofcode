(defun read-file (filename)
  (let ((in (open filename))
        (lines '()))
    (loop for line = (read-line in nil)
      while line do (push line lines))
    (close in)
    (reverse lines)))

(defun split-string (string &optional (delimiter #\Space))
  (flet ((delimiterp (delimiter) (lambda (c) (char= c delimiter))))
  (loop :for beg = (position-if-not (delimiterp delimiter) string)
    :then (1+ end)
    :for end = (and beg (position-if (delimiterp delimiter) string :start beg))
    :when beg :collect (subseq string beg end)
    :while end)))
