;; seaquence.lisp
;; 第11章

;; (format t "I am printing ~s in the middle of this sentense." "foo")

;; (format t "PI can be estimated as ~4f" 3.141593)

;; (format t "PI can be estimated as ~,4f" 3.141593)

;; (format t "PI can be estimated as ~,4f" pi)

;; (format t "Percentage are ~,,2f percent better than franctions" 0.77)

;; (format t "I wish I had ~$ dollars in my bank account." 1000000.2)

;; (progn (princ 22)
;;        (terpri)
;;        (princ 33))

;; (progn (fresh-line)
;;        (princ 22)
;;        (fresh-line)
;;        (fresh-line)
;;        (princ 33))

;; (progn (format t "this is on one line ~%")
;;        (format t "~%this is on another line"))

;; (progn (format t "this is on one line ~&")
;;        (format t "~&this is on another line"))

;; (format t "this will print ~5&on two lines spread far apart")

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(defun random-animal2 ()
  (nth (random 5) '("犬" "ダニ" "虎" "セイウチ" "カンガルー")))

(defun animal-table ()
  (loop repeat 10
     do (format t "~5t~a ~15t~a ~25t~a~%"
                (random-animal)
                (random-animal)
                (random-animal))))

(defun animal-table2 ()
  (loop repeat 10
     do (format t "~30<~a~;~a~;~a~>~%"
                (random-animal)
                (random-animal)
                (random-animal))))


;; 修正時刻： Thu Jun  4 07:31:24 2020
