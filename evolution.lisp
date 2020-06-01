;; evolution.lisp
;; p196

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

;; :test に equal を指定している。この場合、英大小文字を区別する。
(defparameter *plants* (make-hash-table :test #'equal))

;; 植物を生やす関数
;; 乾燥帯よりもジャングルによく生える
;; @param:
;;   left top -- 座標(left, top)
;;   width height -- このエリアの中でランダムに pos を決定する
;; @return: t
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))
;;
;; たとえば random-plant (20 5 3 3) とすると
;; pos は ((20 + (random 3)) . (5 + (random 3))) となる。
;; それが仮に (22 . 6) だったとすると、
;; (setf (gethash (22 . 6) *plants*) t) となる。
;; つまり ハッシュテーブル *plants* の キー(22 . 6) に t が設定される。



;; 植物を生やす関数
;; 敷地全体とジャングルにランダムに植物を生やす。
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
;;
;; apply は 第2引数のリストを解体して 第1引数の関数を適用する。

;; 動物
;; x y -- 座標
;; energy -- 食料の補充なしに何日生き延びられるか。
;; dir -- 移動の方向。 0|1|2
;;                     7|a|3
;;                     6|5|4
;; genes -- 遺伝子をあらわすリスト。dir と対応している。
;;       例) genes (1 1 10 1 1 1 1 1)
;;            dir   0 1 2  3 4 5 6 7
;;       genes によってその動物の移動の傾向が決定されることになる。
(defstruct animal x y energy dir genes)

;; 最初の動物
(defparameter *animals*
  (list (make-animal :x    (ash *width* -1)
                     :y    (ash *height* -1)
                     :energy 1000
                     :dir   0
                     :genes (loop repeat 8
                               collecting (1+ (random 10))))))
;;
;; (loop repeat 8 collecting (1+ (random 10))) の例
;; (6 8 3 7 7 2 10 8)


(defun move (animal)
  (let ((dir (animal-dir animal))
        ;; xスロットの値をxとする
        (x (animal-x animal))
        ;; yスロットの値をyとする
        (y (animal-y animal)))
    ;; xスロットの値を次の式で更新する
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1)))
                                 *width*))
    ;; yスロットの値を次の式で更新する
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0)))
                                 *height*))
    ;; animal-energyを一つ減らす
    (decf (animal-energy animal))))



;; 動物の向きを変える
(defun turn (animal)
  ;; animal-gens の各値を合計して、それを引数にして random する
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;; angleを定義。引数は genes と x。
    (labels ((angle (genes x)
               ;; genes を先頭から順にみていき、x との差を xnu とする
               (let ((xnu (- x (car genes))))
                 ;; もし xnu がマイナスなら
                 (if (< xnu 0)
                     0
                     ;; そうでなければ 引数を変えて angle を実行
                     ;; そのときに 1 を加える。つまり、genes を
                     ;; cdr する度に 1が加算される。そして、
                     ;; (< xnu 0) がマイナスになったときに加算
                     ;; された値が「重み」となる。
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal)
                                               x))
                 8)))))

;; 動物に食べさせる
;; 植物を食べると80ポイント energy がアップする
;; そしてその pos は *plants* から削除される
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


;; 無性生殖
(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    ;; animal-energy が *reproduction-energy* よりも大きければ
    (when (>= e *reproduction-energy*)
      ;; animal-energy を半分にする
      (setf (animal-energy animal) (ash e -1))
      ;; animal をコピーして animal-nu とする
      (let ((animal-nu (copy-structure animal))
            ;; animal-genes をコピーして genes とする
            (genes (copy-list (animal-genes animal)))
            ;; random 8 を mutation(突然変異)とする
            (mutation (random 8)))
        ;; genes の mutation番目を 
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes)
                                             (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))



;; 修正時刻： Mon Jun  1 13:40:14 2020
