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
;; それぞれのノードからつながっているノードのリストを connected に入れて
;; 繋がっていないノードのリストを unconnected に入れている。
;; 
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes)
                                                edge-list))            ; <1>
                      (unconnected (set-difference nodes connected)))  ; <2>
                 (push connected islands)                           ; <3>
                 (when unconnected
                   (find-island unconnected)))))                   ; <4>
      (find-island nodes))
    islands))

;; find-islands の実行例
;; > (find-islands *node-list* *edge-list*)
;; ((23)
;;  (15 9 20 30 4 13 5 22 8 19 18 11 17 21 28 10 16 27 26 2 12 6 3 29 7 24 25 14 1))
;;
;; この実行例の場合、<1>で多数のnodeが connected にセットされている。
;; 23 は unconnected にセットされている。そしてもう一度 (find-island
;; unconnected) を実行している。が、その時、<1> で 23 だけが返され、
;; (23) として connected にセットされる。
;; そして、<3> で islands にプッシュされる。
;; islands には、すでに (15 9 ... 14 1) のリストがプッシュされているの
;; で、結果として、islands には ((23) (15 9 ... 14 1)) という二つのリ
;; ストが含まれることになる。



(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;; デバッグ用に *islands* を作成してみる。
;;
;; > (defparameter *islands* (find-islands *node-list* *edge-list*))
;; *ISLANDS*
;;
;; > *islands*
;; ((23) (22)
;;  (30 20 6 10 17 27 12 4 14 29 24 11 15 2 8 13 7 3 19 25 16 28 5 26
;; 9 21 18 1))
;;
;; 今回は上のような *islands* のリストができた。
;; 式の中の値をそれぞれ調べてみた。
;;
;; > (cdr *islands*)
;; ((22)
;;  (30 20 6 10 17 27 12 4 14 29 24 11 15 2 8 13 7 3 19 25 16 28 5 26 9 21 18 1))
;;
;; > (caar *islands*) -- caar : リストに含まれる先頭のリストの要素
;; 23
;; > (car *islands*)
;; (23)
;; > (caadr *islands*) -- caadr : リストに含まれる二つめのリストの先頭要素
;; 22
;;
;; (when (cdr islands) とあるから、islands の中に二つのリストがある場
;; 合になる。
;; その先頭は孤島である。その孤島と二つめのリストの先頭要素を
;; edge-pair関数で繋ぐ。
;;
;; 次に同じことを (cdr islands) について行う。
;; 今回の場合、先頭の (23) は、次の (22) とペアになって (23 . 22) (22
;; . 23) となった。
;; (cdr islands) は、((22) (30 20 6 ... 1)) である。
;; (caar islands) は 22、(caadr islands) は 30 となる。
;; edge-pair関数で、(22 . 30) (30 . 22) となる。
;;
;; そして、また (cdr islands) について行う。
;; 今回の (cdr islands) は、(30 20 .... 1) である。
;; これは、(when (cdr islands) のところで 偽 となるので、以下の処理は
;; 行われない。結果、(connect-with-bridges *islands*) は
;;   (connect-with-bridges *islands*)
;;   ((23 . 22) (22 . 23) (22 . 30) (30 . 22))   となる。
;;
;; つまり、今回の *islands* では、孤島が二つ (23 と 22) できたので、
;; 23 と 22 と 大きな島の先頭の 30 とをつなぐペアが作成されたのである。


(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))

;; 今回の場合、*edge-list* には、23 と 22 が含まれていないので、
;; (connect-with-bridge *islands*) で、ペアが作成されたので、
;; append関数で、一つのリストにする。
;;
;; (append (connect-with-bridges *islands*) *edge-list*)
;; ((23 . 22) (22 . 23) (22 . 30) (30 . 22) (9 . 26) (26 . 9) (24 . 29) (29 . 24)
;;  (25 . 19) (19 . 25) (4 . 12) (12 . 4) (8 . 2) (2 . 8) (29 . 14) (14 . 29)
;;  (24 . 11) (11 . 24) (25 . 16) (16 . 25) (26 . 5) (5 . 26) (18 . 1) (1 . 18)
;;  (27 . 24) (24 . 27) (28 . 5) (5 . 28) (3 . 7) (7 . 3) (7 . 13) (13 . 7)
;;  (14 . 4) (4 . 14) (25 . 3) (3 . 25) (29 . 12) (12 . 29) (29 . 16) (16 . 29)
;;  (13 . 1) (1 . 13) (5 . 3) (3 . 5) (24 . 12) (12 . 24) (30 . 1) (1 . 30)
;;  (21 . 18) (18 . 21) (26 . 6) (6 . 26) (24 . 30) (30 . 24) (25 . 30) (30 . 25)
;;  (2 . 9) (9 . 2) (9 . 21) (21 . 9) (10 . 17) (17 . 10) (5 . 24) (24 . 5)
;;  (6 . 10) (10 . 6) (15 . 21) (21 . 15) (15 . 11) (11 . 15) (8 . 7) (7 . 8)
;;  (6 . 24) (24 . 6) (3 . 26) (26 . 3) (27 . 17) (17 . 27) (30 . 20) (20 . 30)
;;  (21 . 15) (15 . 21) (10 . 20) (20 . 10) (16 . 28) (28 . 16) (15 . 7) (7 . 15)
;;  (19 . 9) (9 . 19) (1 . 25) (25 . 1))
;;
;; これで、全ての地点をつなぐルートが作成されたことになる。

;;===================================================================
;; p132
;;
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; 実行例
;;
;; (defparameter *all-routes* (append (connect-with-bridges *islands*)
;; *edge-list*))
;;
;; 全ての地点をつなぐルートを *all-routes* という変数名で作成したとす
;; る。
;;
;; > (defparameter *cops* (remove-if-not (lambda (x)
;;                                        (zerop (random *cop-odds*)))
;;                                      *all-routes*))
;; *COPS*
;;
;; > *cops*
;; ((7 . 13) (3 . 5) (10 . 17) (7 . 8) (25 . 1))
;;
;; という結果になる。



(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1
                                                           edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1
                                                             node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))



;;;===================================================================
;;; 今までの関数を動かしてみる。
;;;

(defparameter *edge-list* (make-edge-list))

;; 1から30までのnodesリストをつくる
(defun make-node-list ()
  (loop for n from 1 to *node-num*
       collect n))

(defparameter *node-list* (make-node-list))

(defparameter *islands* (find-islands *node-list* *edge-list*))

(defparameter *all-routes* (append (connect-with-bridges *islands*) *edge-list*))

;;; 修正時刻： Thu May 21 08:41:05 2020
