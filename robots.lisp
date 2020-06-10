;; robots.lisp
;;
;; 盤面は 64 x 16
;; pos = 544 は 盤面のちょうど真ん中
;; @ -- player
;; A -- monster(robots)
;; # -- scrap
;;
(defun robots ()
  (loop named main
     with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                         (d .   1) (z .  63) (x .  64) (c . 65))
     for pos = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                 (force-output)
                 (let* ((c (read))
                        (d (assoc c directions)))
                   (cond (d (+ pos (cdr d)))
                         ((eq 't c) (random 1024))
                         ((eq 'l c) (return-from main 'bye))
                         (t pos))))
     for monsters = (loop repeat 10
                       collect (random 1024))
     then (loop for mpos in monsters
             ;; (count mpos monsters) --- リストmonsters の中の mpos
             ;;   と等しい要素の個数
             ;; monsters の中に mpos が複数あれば
             collect (if (> (count mpos monsters) 1)
                         mpos
                         ;; (sort LIST '< :key #'car) --
                         ;;   LIST -- ((40 . 896) (55 . 912)...) とす
                         ;;   ると、40 55 をキーとしてソートすることに
                         ;;   なる。つまり、pleyerとmonsterの間の距離,
                         ;;   xとyを足した値の昇順でソートされる。
                         ;;   つまり、pleyerとの距離の近いもの順。
                         (cdar (sort (loop for (k . d) in directions
                                        ;; monsters も player と同じ
                                        ;; 方向に動く
                                        for new-mpos = (+ mpos d)
                                        ;; monsterの位置とplayerの位置のX座標の差と
                                        ;; y座標の差の和
                                        ;; (40 . 896) という cons ができる
                                        collect (cons (+ (abs (- (mod new-mpos 64)
                                                                 (mod pos 64)))
                                                         ;; (ash n -6) -- 64分の1
                                                         ;; つまり、64がいくつあ
                                                         ;; るかがわかる
                                                         (abs (- (ash new-mpos -6)
                                                                 (ash pos -6))))
                                                      new-mpos))
                                     '<
                                     :key #'car))))
     ;; monsters の mposをひとつひとつ調べて、全ての mpos が複数あれば
     ;; pleyerの勝ち。
     when (loop for mpos in monsters
             always (> (count mpos monsters) 1))
     return 'player-wins
     do (format t
                "~%|~{~<|~%|~,65:;~A~>~}|"
                (loop for p
                   below 1024
                   collect (cond ((member p monsters)
                                  (cond ((= p pos) (return-from main 'player-loses))
                                        ((> (count p monsters) 1) #\#)
                                        (t #\A)))
                                 ((= p pos)
                                  #\@)
                                 (t
                                  #\ ))))))


(defparameter *directions* '((q . -65) (w . -64) (e . -63) (a . -1)
                               (d .   1) (z .  63) (x .  64) (c . 65)))

(defparameter *monsters* (loop repeat 10 collect (random 1024)))



;; 修正時刻： Thu Jun 11 08:24:48 2020
