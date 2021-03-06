;; orcBattle.lisp

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    ;; (princ "You have been killed. Game Over."))
    (princ "あなたは殺されてしまった。ゲームオーバー"))
  (when (monsters-dead)
    ;; (princ "Congraturations! You have vanquished all of your foes.")))
    (princ "おめでとう。あなたは全ての敵を打ち負かした。")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  ;; (princ "You are a valiant knight with a health of ")
  (princ "あなたは勇敢な兵士だ。あなたの体力は ")
  (princ *player-health*)
  ;; (princ ", and a agility of ")
  (princ " で、敏捷さは ")
  (princ *player-agility*)
  ;; (princ ", and a strength of ")
  (princ " で、攻撃力は ")
  (princ *player-strength*)
  (princ " である。"))


(defun player-attack ()
  (fresh-line)
  ;; stab -- 突く
  ;; double swing -- ダブルスイング
  ;; rounchouse swing -- なぎはらう
  ;; (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (princ "攻撃の種類は？: [s]突く [d]ダブルスイング [r]なぎはらう： ")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         ;; (princ "Your double swing has a strength of ")
         (princ "あなたのダブルスイングの攻撃は ")
         (princ x)
         (princ " であった。")
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  ;; (princ "Monster #:")
  (princ "モンスターナンバー #: ")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn ;; (princ "That is not a valid monster number.")
          (princ "そのモンスターナンバーは適切ではない。")
          (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn ;; (princ "That monster is already dead.")
                (princ "そのモンスターはすでに死んでいる。")
                (pick-monster))
              m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  ;;  (princ "Your foes:")
  (princ "あなたの攻撃: ")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               ;; (princ "**dead**")
               (princ "**死んだ**")
               (progn ;;(princ "(Health=")
                 (princ "(体力=")
                 (princ (monster-health m))
                 (princ ") ")
                 (monster-show m))))
         *monsters*)))

;; p174
;; monster
(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn ;; (princ "You kill the ")
        (princ "あなたは ")
        (princ (type-of m))
        (princ " を倒した! "))
      (progn (princ "あなたは ")
             (princ (type-of m))
             (princ " を攻撃し, その体力を ")
             (princ x)
             (princ " ポイント奪った。 "))))
      ;; (progn (princ "You hit the ")
      ;;        (princ (type-of m))
      ;;        (princ ", knocking off ")
      ;;        (princ x)
      ;;        (princ " health points! "))))

(defmethod monster-show (m)
  ;; (princ "A fierce ")
  (princ "獰猛な ")
  (princ (type-of m)))

(defmethod monster-attack (m)
  (declare (ignore m)))

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  ;; (princ "A wicked orc with a level ")
  (princ "攻撃力 ")
  (princ (orc-club-level m))
  ;; (princ " club"))
  (princ " の棍棒を持つ邪悪なオーク。"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    ;; (princ "An orc swings his club at you and knocks off ")
    (princ "オークが棍棒であなたを攻撃し、あなたの体力を ")
    (princ x)
    ;;    (princ " of your health points. ")
    (princ " ポイント奪った。")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  ;; (princ "A malicious hydra with ")
  (princ "凶悪なヒドラ。頭が ")
  (princ (monster-health m))
  ;; (princ " heads."))
  (princ " つある。"))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      ;; (princ "The corpse of the fully decapitated and decapacitated
      ;; hydra falls to the floor!")
      (princ "頭が切り落とされ、ヒドラの死体が床にある。")
      (progn ;; (princ "You lop off")
        (princ "あなたは ")
        (princ x)
        (princ " つの頭を切り落とした。"))))
        ;; (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    ;; (princ "A hydra attacks you with ")
    (princ "ヒドラが ")
    (princ x)
    (princ " つの頭であなたを攻撃した。また一つ頭が増えた。")
    ;; (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  ;; (princ "A slime mold with a sliminess of ")
  (princ "粘性度 ")
  (princ (slime-mold-sliminess m))
  (princ " のスライム。"))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    ;; (princ "A slime mold wraps arounc your legs and decreses your
    ;; agility by ")
    (princ "スライムがあなたの脚を包み込み、あなたの敏捷度を ")
    (princ x)
    (princ " 奪った! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      ;; (princ "It also squirts in your face, taking away a health point! ")
      (princ "あなたの顔面に液体を吹きかけ、あなたの体力を奪った!")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           ;; (princ "A brigand hits you with his slingshot, taking
           ;; off 2 health points! ")
           (princ "ブリガンドがスリングショットであなたを攻撃し、あなたの体力を 2ポイント奪った! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           ;; (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (princ "ブリガンドがあなたの脚を鞭で掴み、あなたの敏捷度を 2ポイント奪った!")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           ;; (princ "A brigand cuts your arm with his whip, taking
           ;; off 2 stringth points! ")
           (princ "ブリガンドがあなたの腕を鞭で掴み、あなたの攻撃力を 2ポイント奪った!")
           (decf *player-strength* 2)))))


;; 修正時刻： Sat May 30 21:01:45 2020
