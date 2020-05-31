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



;; 修正時刻： Sun May 31 14:24:35 2020
