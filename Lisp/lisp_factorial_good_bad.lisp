;; Good Code Example: Calculating factorial using recursion
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Bad Code Example:  Calculating factorial with unnecessary complexity and readability issues.
(defun bad-factorial (n)
  (let ((result 1))
    (loop for i from 1 to n do
      (setf result (* result i)))
    result))

