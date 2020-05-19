;;; congestion.lisp
;;; p127
;;;

(load "graph")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
;; *cop-odds* -- 検問 それぞれの道路に15分の1の確率
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                       collect (edge-pair (random-node)(random-node)))))


;;; 修正時刻： Tue May 19 12:31:49 2020
