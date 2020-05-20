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

;;--------------------------------------------------------------------------
;; (make-edge-list)を実行してみた。
;; *edge-list* というグローバル変数に入れてみた。
;;
;; > (defparameter *edge-list* (make-edge-list))
;; *EDGE-LIST*
;;
;; > *edge-list*
;; ((12 . 24) (24 . 12) (5 . 30) (30 . 5) (4 . 28) (28 . 4) (26 . 3) (3 . 26)
;;  (10 . 14) (14 . 10) (29 . 4) (4 . 29) (29 . 15) (15 . 29) (30 . 27) (27 . 30)
;;  (16 . 6) (6 . 16) (26 . 13) (13 . 26) (30 . 20) (20 . 30) (2 . 11) (11 . 2)
;;  (26 . 20) (20 . 26) (27 . 26) (26 . 27) (27 . 4) (4 . 27) (6 . 13) (13 . 6)
;;  (15 . 28) (28 . 15) (24 . 4) (4 . 24) (15 . 26) (26 . 15) (21 . 3) (3 . 21)
;;  (6 . 10) (10 . 6) (24 . 17) (17 . 24) (10 . 28) (28 . 10) (20 . 10) (10 . 20)
;;  (2 . 27) (27 . 2) (27 . 18) (18 . 27) (25 . 22) (22 . 25) (13 . 9) (9 . 13)
;;  (30 . 9) (9 . 30) (22 . 16) (16 . 22) (7 . 10) (10 . 7) (26 . 23) (23 . 26)
;;  (25 . 18) (18 . 25) (1 . 19) (19 . 1) (4 . 7) (7 . 4) (7 . 3) (3 . 7) (2 . 1)
;;  (1 . 2) (11 . 19) (19 . 11) (2 . 5) (5 . 2) (8 . 11) (11 . 8) (29 . 22)
;;  (22 . 29) (5 . 22) (22 . 5) (4 . 3) (3 . 4) (25 . 24) (24 . 25))
;;--------------------------------------------------------------------------


;; ある地点からどの地点につながっているかをリストする
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))
;;
;; 実行例
;; (direst-edges 2 *edge-list*)
;; ((2 . 11) (2 . 27) (2 . 1) (2 . 5))


;;
;; get-connected
;;
;; ある地点から次の地点、そのまた次の地点へと、つながっている地点のリ
;; ストが作成される
;;
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 ;; edge -- (2 . 11)という形をしている
                 (mapc (lambda (edge)
                         ;; (cdr edge)を新しいnodeとしてtraverseする
                         (traverse (cdr edge)))
                       ;; あるnodeが直接つながっているedgeのリスト
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;;
;; 実行例
;; > (get-connected 2 *edge-list*)
;; (17 23 20 9 13 16 6 14 10 7 21 3 26 29 15 28 4 12 24 18 25 22 5 30 27 8 1 19 11
;;  2)
;; これは 2 という地点からつながっている地点のリストである
;;


;; get-connected関数を使って全ノードを調べる。
;; それぞれのノードからつながっているノードのリストを connected に入れて繋がっていないノードのリストを unconnected に入れている。
;; 
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes)
                                                edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))



(defun connected-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))

;; 1から30までのnodesリストをつくる
(defun make-node-list ()
  (loop for n from 1 to *node-num*
       collect n))

;;; 修正時刻： Wed May 20 10:21:15 2020
