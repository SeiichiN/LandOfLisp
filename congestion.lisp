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

;; 1..30の数値を返す
(defun random-node ()
  (1+ (random *node-num*)))

;; (a . b)(b . a) のリストを作る
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;; applyを使うと、全部をひっくるめた大きなリストになる
;; ((28 . 6) (6 . 28) (24 . 16) (16 . 24) ...)
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                       collect (edge-pair (random-node)(random-node)))))

;; これだと、(((28 . 6)(6 . 28)) ((24 . 16)(16 . 24)) ...) というリス
;; トになってしまう
;; (defun make-edge-list ()
;;   (append (loop repeat *edge-num*
;;              collect (edge-pair (random-node)(random-node)))))


;;; 修正時刻： Tue May 19 16:38:11 2020
