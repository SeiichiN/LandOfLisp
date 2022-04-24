;; clojure

(defparameter *foo* (lambda ()
                      5))

(funcall *foo*)


(defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))

(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))



;; 修正時刻: Wed 2022/04/20 06:02:10
