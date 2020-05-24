;;; congestion.lisp
;;; p127
;;;

(load "graph")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *player-pos* nil)
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
;; make-city-edges
;; 全ての道を作成する
;; cops がどこにいるかも情報として入っている。
;;
;; nodes -- (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
;; edge-list -- すべての島をブリッジでつないだ edge のリスト。
;;         僕の作ったグローバル変数では *all-routes* になる。
;; cops -- ランダムに選択された cops という edge のリスト。検問所のあ
;; るリスト。
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x))           ; <2>
                                (zerop (random *cop-odds*)))   ; <1>
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
;;
;; <1> のところ。
;; (random *cop-odds*) で 0...14 の範囲で乱数が生成される。
;; 0 のとき (zerop (random *cop-odds*)) は T になる。
;; その場合のみ、(remove-if-not ...) で、その要素は cops に入れられる。
;; lambda (x) の x には、edge-list の各要素が順に入り、zerop... が T
;; になった場合のみ、cops にセットされるのである。
;;
;; <2> lamda (x) の x は使わない。コンパイラから警告が出るので、
;;   (declare (ignore x)) で警告を無視するようにコンパイラに伝える。


;; ある地点から、どの地点につながっているかを alist にする
;; たとえば 地点13 は、14 と 16 につながっているから
;; (13 (14) (16)) というリストになる。
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))             ; <3>
                          (remove-duplicates (direct-edges node1 ; <2>
                                                           edge-list)
                                             :test #'equal))))
          ;; (22 14 13 12 11 ... 9 29 24 23 18 19 25 28)
          (remove-duplicates (mapcar #'car edge-list))))   ; <1>

;; <1> edge-list すなわち *all-routes* の car をリストにしたもの
;;   (22 14 13 12 11 6 3 26 27 17 30 21 10 20 16 15 1 5 7 2 4 8 9
;;    29 24 23 18 19 25 28)
;;
;; <2> -- <1>のそれぞれの node について direct-edge関数を実行
;;     (direct-edges 22 *all-routes*) -- ((22 . 9))
;;     (direct-edges 14 *all-routes*) -- ((14 . 13))
;;     (direct-edges 13 *all-routes*) -- ((13 . 14) (13 . 16))
;;     remove-duplicates関数で重複要素を削除している。
;;
;; 例えば 地点22 から direct-edge で直接つながっている edge は
;; ((22 . 9)) である。
;; 同様に 地点14 の場合は ((14 . 13))
;; 同様に 地点13 の場合は ((13 . 14) (13 . 16)) となる。
;; <3> -- (22 . 9) なら (9)。(14 . 13) なら (13)。
;;   (13 . 14) (13 . 16) なら (14) (16) となる。
;; このとき、node1 は 13 なので、(cons node1 (mapcar ...)) は、
;; (13 (14) (16)) となる。
;;
;; 実行例
;; > (edges-to-alist *all-routes*)
;; ((22 (9)) (14 (13)) (13 (14) (16)) (12 (29) (30)) (11 (8)) (6 (24) (7))
;;  (3 (5) (29) (24)) (26 (18) (27)) (27 (1) (26)) (17 (5) (30))
;;  (30 (12) (25) (2) (17)) (21 (1) (10) (15) (20)) (10 (21) (15)) (20 (21) (16))
;;  (16 (13) (20)) (15 (21) (10) (28)) (1 (27) (5) (21) (9))
;;  (5 (8) (3) (1) (17) (25)) (7 (6) (19) (8)) (2 (30) (4)) (4 (25) (2))
;;  (8 (5) (18) (11) (7) (29)) (9 (22) (1) (18)) (29 (3) (12) (8) (24))
;;  (24 (28) (6) (3) (29)) (23 (25)) (18 (8) (26) (9) (19)) (19 (7) (18))
;;  (25 (4) (30) (5) (23) (28)) (28 (24) (15) (25)))


;; edge-alist -- edges-to-alist関数で作成した alist
;; edges-wigh-cops -- 警官のいる道。
;;    make-city-edges関数の中で作成。たとえば、
;;    ((7 . 13) (3 . 5) (10 . 17) (7 . 8) (25 . 1))
;;    というリストになる。
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)                                     ; <1>
            (let ((node1 (car x))                         ; <2>
                  (node1-edges (cdr x)))                  ; <3>
              (cons node1                                 ; <7>
                    (mapcar (lambda (edge)                ; <4>
                              (let ((node2 (car edge)))   ; <5>
                                (if (intersection (edge-pair node1  ; <6>
                                                             node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

;; <1> -- x が仮に (13 (14) (16)) だとする。
;; <2> -- node1 は 13 になる。
;; <3> -- node1-edges は ((14) (16)) となる。
;; <4> -- edge には (14)、(16) と順に入る。
;; <5> -- node2 は、たとえば 14 になる。
;; <6> -- node1 が 13、node2 が 14 なら (13 . 14) とうペアが作成され、
;;     警官のいる edge-list の中にそのペアがあるかとうかを
;; intersection関数を使って調べ、もしあれば (14 cops) を、なければ
;; (14) を mapcar の返り値として返す。
;; それを <7> で (13 14 cops) という形にする。
;; すなわち、(cons 13 '(14 cops)) -> (13 14 cops) となる。

;;----------------------------------------------------------------
;; (make-city-edges) の実行例
;; > (make-city-edges)
;; ((23 (22)) (2 (15)) (21 (14)) (29 (11)) (24 (11) (28 COPS) (15))
;;  (22 (23) (18) (10) (12 COPS)) (26 (17) (20) (18)) (15 (2) (24) (30))
;;  (19 (10) (20)) (11 (24) (29) (13 COPS)) (13 (1) (11 COPS)) (3 (12) (8 COPS))
;;  (18 (16) (22) (26) (1)) (1 (9) (12) (13) (18)) (14 (21) (8) (6))
;;  (6 (8 COPS) (14)) (12 (27 COPS) (1) (22 COPS) (3) (25)) (25 (4) (20) (12))
;;  (5 (4) (27) (7)) (9 (1) (8)) (28 (24 COPS) (20)) (20 (26) (19) (25) (28))
;;  (17 (26) (7)) (7 (5) (17)) (4 (5) (25) (30)) (30 (15) (16) (4))
;;  (27 (10) (12 COPS) (5) (8 COPS)) (8 (6 COPS) (14) (3 COPS) (9) (27 COPS))
;;  (10 (27) (19) (22) (16)) (16 (18) (30) (10)))
;;----------------------------------------------------------------


;; p135

;; 隣の地点のリストを返す関数
;; node -- ある地点。たとえば 19
;; edge-alist -- *all-city-edges*
;;
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

;; > (neighbors 19 *all-city-edges*)
;; (21 13 20)

;; > (assoc 19 *all-city-edges*)
;; (19 (21 COPS) (13) (20))

;; a と b の距離が 1 であるかどうかを調べる関数
;; 1であれば b を含むリストが返る。
;; 1でなければ nil が返る。
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

;; CL-USER> (within-one 19 21 *all-city-edges*)
;; (21 13 20)

;; a と b の距離が2以下であるかを調べる関数
;; 2以下であれば、そのリストが返る
;; そうでなければ nil が返る
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))


;;
;; *worm-num* -- 3。突厥ぼたるの数
;; random-node -- 1...30の間でランダムな数を生成
;;
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus))
                             ((within-two n wumpus edge-alist)
                              '(blood!)))
                       (cond ((member n glow-worms)
                              '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       (when (some #'cdr (cdr (assoc n edge-alist)))
                         '(sirens!))))))

;; wumpus -- 1...30のうちのどれかになる。
;; ggow-worms -- 1...30のうちの3つの数。
;; loopの処理 -- 1...30まで順に次の処理をおこなう。
;;     リストを作成。 (1 A B C)
;;       A -- もし n が wumpus と同じなら '(wumpus) になる。
;;            そうでなければ、n と wumpus の距離が 2以下であれば、'(blood!) になる。
;;       B -- もし n が glow-worms に含まれているならば、'(glow-worm) になる。
;;            そうでなければ、n と 3つのglow-wormsのどれか一つ以上との距離が
;;            1以下であれば '(lights!) になる。
;;       C -- (some #'cdr (cdr (assoc n edge-alist))) -- copsがあるかどうかを
;;            調べて、もしあれば、'(sirens!) になる。
;; 

;; 完全な地図を描く関数
;; あとに new-game という関数を定義しているので、この名前にしておいた。
(defun new-game-comp ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

;; p139 既知のノード
(defun known-city-nodes ()
  (mapcar
   ;; そのnodeが *visited-node* なら
   ;; そのnodeについての詳しい情報を n に入れる
   ;; また、プレイヤーの場所なら * をつける
   ;; *visited-node* でなければ ? をつける
   (lambda (node)
     (if (member node *visited-nodes*)
         (let ((n (assoc node *congestion-city-nodes*)))
           (if (eql node *player-pos*)
               (append n '(*))
               n))
         (list node '?)))
   (remove-duplicates
    (append *visited-nodes*
            ;; リスト*visited-nodes* のそれぞれが
            ;; どのnodeにつながっているかをリストにする
            (mapcan (lambda (node)
                      (mapcar #'car
                              (cdr (assoc node
                                          *congestion-city-edges*))))
                    *visited-nodes*)))))


;; p139
;; まだ訪れていない道にいる警官のサイレンの情報を取り除いたエッジの
;; alist
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node
                  (mapcar (lambda (x)
                            (if (member (car x) *visited-nodes*)
                                x
                                (list (car x))))
                          ;; node からつながっている道のリスト
                          (cdr (assoc node
                                      *congestion-city-edges*)))))
          *visited-nodes*))


;; 既知のノードとエッジだけを含むグラフを描く
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))


;; 新しくゲームを開始する
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-known-city))


;; 街を歩き回る
(defun walk (pos)
  (handle-direction pos nil))

;; ワンプスの場所に攻撃をかける
(defun charge (pos)
  (handle-direction pos t))

;; pos -- 次に行きたい地点
;; charging -- 攻撃をしかけるかどうか (t / nil)
(defun handle-direction (pos charging)
  ;; posがリストに存在すればその pos が返るのでそれが edge になる
  ;; なければ nil が返るので、それが edge になる
  (let ((edge (assoc pos
                     ;; 今いる地点の次の地点のリストを得る
                     ;; ((27) (12) (14))
                     (cdr (assoc *player-pos*
                                 *congestion-city-edges*)))))
    ;; edge が nil でなければ
    (if edge
        ;; handle-new-place を実行
        (handle-new-place edge pos charging)
        ;; nil ならば表示
        (princ "That location does not exist!"))))


;; プレイヤーが新しい場所に移動したときに呼ばれる関数
;; edge -- 次に行ける地点
;; pos -- 次に行きたい地点
;; charging -- 攻撃するかどうか t / nil
(defun handle-new-place (edge pos charging)
  ;; node -- 次に行きたい地点の情報
  (let* ((node (assoc pos *congestion-city-nodes*))
         ;; そのnodeが 'glow-worm を含んでいて
         ;; しかも まだ訪れていない地点なら
         ;; has-worm は t となる。
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    ;; posが *visited-nodes* に無ければ追加する
    (pushnew pos *visited-nodes*)
    ;; 現在の地点を pos とする
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge)
           (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node)
           (if charging
               (princ "You found the Wumpus!")
               (princ "You ran into the Wumpus.")))
          ;; node に 'wumpus がいないのに charging したら wasted
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm
           (let ((new-pos (random-node)))
             (princ "You ran into a Glow Worm Gang! You're now at ")
             (princ new-pos)
             (handle-new-place nil new-pos nil))))))


;;;===================================================================
;;; 今までの関数を動かしてみる。
;;; テスト用のグローバル変数

(defparameter *edge-list* (make-edge-list))

;; 1から30までのnodesリストをつくる
(defun make-node-list ()
  (loop for n from 1 to *node-num*
       collect n))

(defparameter *node-list* (make-node-list))

(defparameter *islands* (find-islands *node-list* *edge-list*))

(defparameter *all-routes* (append (connect-with-bridges *islands*) *edge-list*))

;; copsも含めた全ての道を作成する
(defparameter *all-city-edges* (make-city-edges))
;; *ALL-CITY-EDGES*
;;
;; CL-USER> *all-city-edges*
;; ((23 (4)) (4 (23) (15)) (24 (27)) (27 (24) (11)) (5 (25))
;;  (21 (11) (17) (19 COPS)) (26 (20)) (28 (9)) (17 (21) (9) (6)) (29 (7) (11))
;;  (30 (13) (3) (1)) (10 (14) (18) (16)) (1 (18) (30) (20)) (12 (9))
;;  (19 (21 COPS) (13) (20)) (9 (13) (17) (8 COPS) (28) (25) (12) (14)) (2 (25))
;;  (25 (5) (14 COPS) (9) (8) (2)) (22 (20) (7)) (15 (4) (18))
;;  (13 (30) (9) (19) (6)) (6 (17) (13)) (3 (30) (11)) (16 (11) (10) (8 COPS))
;;  (14 (10) (25 COPS) (9) (7)) (7 (29) (22) (14)) (18 (1) (10) (15) (8))
;;  (8 (9 COPS) (25) (16 COPS) (18) (11)) (20 (26) (1) (19) (22) (11))
;;  (11 (16) (27) (21) (29) (3) (8) (20)))
;;
;; 12 を起点とする道
;; CL-USER> (assoc 12 *all-city-edges*)
;; (12 (9))
;;
;; 19 を起点とする道
;; CL-USER> (assoc 19 *all-city-edges*)
;; (19 (21 COPS) (13) (20))
;;
;; 19 を起点として、目的地はどうなるか？
;; CL-USER> (cdr (assoc 19 *all-city-edges*))
;; ((21 COPS) (13) (20))
;;
;; 19 を起点として 21 を目的地とすると、その道に警官はいるか？
;; CL-USER> (cdr (assoc 21 (cdr (assoc 19 *all-city-edges*))))
;; (COPS)
;;
;; 22 が目的地ならどうか？
;; CL-USER> (cdr (assoc 20 (cdr (assoc 19 *all-city-edges*))))
;; NIL

;; 地図の完成
(defparameter *all-ciry-nodes* (make-city-nodes *all-city-edges*))
;;
;; ((1 BLOOD! SIRENS!) (2 LIGHTS!) (3) (4) (5 GLOW-WORM) (6 SIRENS!) (7 SIRENS!)
;;  (8 SIRENS!) (9) (10) (11 LIGHTS!) (12 LIGHTS!) (13) (14 LIGHTS!) (15)
;;  (16 SIRENS!) (17 LIGHTS!) (18) (19 BLOOD! LIGHTS!) (20) (21 LIGHTS!)
;;  (22 GLOW-WORM) (23 BLOOD! SIRENS!) (24) (25 LIGHTS! SIRENS!) (26 LIGHTS!)
;;  (27 WUMPUS SIRENS!) (28) (29 GLOW-WORM) (30 SIRENS!))



;;; 修正時刻： Sun May 24 10:02:20 2020
