;; play.lisp

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;;
;;; 盤面配列をつくる
;;;
;; make-array -- 配列を作成する。
;; :initial-contents lst -- 要素の初期値を lst とする
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;;;
;;; 盤面をランダムにつくり出す
;;; 
;; n を 0 から *board-hexnum* (< 4) までくり返す
;; collect -- listにして返す
;; (list (random *num-players*) (1+ (random *max-dice*)))
;; --> たとえば '((0 2) (1 0) (0 1) (1 3))
(defun gen-board()
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))

;; プレイヤーの文字表現
;; 0 -> a
;; 1 -> b
(defun player-letter (n)
  (code-char (+ 97 n)))

;;;
;;; 盤面を描く
;;;
;; repeat n -- 指定した回数 n だけ繰り返す
;; y: 0 0 1 1 
;; x: 0 1 0 1
;; (aref board 0) -- 配列 board の index#0 -- board[0]
;; (aref board 1)
;; (aref board 2)
;; (aref board 3)
(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (- *board-size* y)   ; 2, 1, 0
                  do (princ "  "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))
;;;
;;; 木構造をつくる
;;;
;;; 与えられた初期条件から、全ての可能な指し手を表現する木構造をつくる。
;;; この関数は、ゲームの最初に一度だけ呼ばれる。呼ばれると、再帰を使って
;;; ゲームの可能な動きを全て、最終的に勝敗が決するまで辿る。ゲームの他の
;;; 部分がルールに従うためには、この木構造を辿るだけでよい。
;;;
;; board -- 盤面の状態(配列表現)
;; player -- 現在手番のプレーヤー(0, 1)
;; spare-dice -- 現在の手番でプレーヤーがいくつサイコロを獲得したか。
;;               これは補給を計算するのに必要。
;; first-move -- プレーヤの最初の指し手かどうか。(bool値)
;;   ルールによると、プレーヤーは
;;   全ての自分のマス目から隣の敵のマス目に攻撃できる。さらに、プレー
;;   ヤーは必ず1回何かの指し手を指さなくてはならない。
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        ;; 相手に手番をわたす
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          ;; 攻撃の指し手
                          (attaking-moves board player spare-dice))))

;;;
;;; 相手に手番を渡すという指し手をゲーム木に加える関数
;;;
;; board -- 盤面
;; player -- 現在のプレーヤー(0, 1)
;; spare-dice -- 獲得したダイス(予備ダイス)
;; first-move -- 最初の指し手であるかどうか(t / nil)
;; moves -- 現在までに集められた可能な指し手
;;
(defun add-passing-move (board player spare-dice first-move moves)
  ;; もしこれが自分の手番の最初の指し手ならば
  (if first-move
      ;; そのまま返す
      moves
      ;; さもなくば、指し手を moves に追加する
      (cons (list nil  ; 手の記述。単に手番を終了するだけなので nil
                  ;; この手が指された後にあり得る全ての可能性を表す
                  ;; ゲーム木
                  (game-tree
                   ;; 次の盤面(board)
                   (add-new-dice board player (1- spare-dice))
                   ;; 次のプレーヤー
                   (mod (1+ player) *num-players*)
                   ;; spare-dice(予備のダイス、獲得したダイス) はゼロ
                   0
                   ;; 最初の指し手である
                   t))
            moves)))

;;;
;;; 可能な攻撃の指し手をゲーム木に追加する関数
;;;
;; board
;; cur-player -- 0, 1
(defun attaking-moves (board cur-player spare-dice)
  ;; player -- ゲーム版の指定場所(pos)の占有者を返すローカル関数
  ;; pos -- 配列の index (0, 1, 2)
  (labels ((player (pos)
             (car (aref board pos)))
           ;; dice -- 指定場所(pos) にあるサイコロの数を返すローカル関数
           (dice (pos)
             (cadr (aref board pos))))
    ;; src -- 場所
    (mapcan (lambda (src)
              ;; 攻撃元場所src の占有者が 現在のプレーヤーであれば
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          ;; 攻撃先場所dst の占有者が 現在のプレーヤーでなくて、 
                          (when (and (not (eq (player dst) cur-player))
                                     ;; 攻撃先のダイスの数が攻撃元よりも少ければ
                                     (> (dice src) (dice dst)))
                                (list
                                 (list (list src dst)
                                       ;; board-attack -- src から dst を攻撃した
                                       ;;   あとのボードの配列を返す
                                       (game-tree (board-attack board cur-player
                                                                src dst (dice src))
                                                  cur-player
                                                  (+ spare-dice (dice dst))
                                                  nil)))))
                        ;; 隣接するマスを見つける。リストにして返す
                        (neighbors src))))
            (loop for n below *board-hexnum*
               collect n))))

;;;
;;; ある六角マスに隣接するマスを見つける関数
;;;
;; pos - 0...3 のいずれかの整数
;; 返り値 -- 隣接するマスのリスト
;;
(defun neighbors (pos)
  ;; up は pos の上にあたるマス。down は下にあたるマス。
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    ;; append以下で隣マスのリストを作成し、その各要素を p としてループする。
    (loop for p in (append (list up down)  ;; 上のマスと下のマスをリストとする。
                           ;; pos が左端でなかったら、左上(1- up)のマスと
                           ;; 左隣(1- pos) のマスも隣マスとする
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           ;; pos が左端だったら、その右隣(1+ pos) と
                           ;; 右下(1+ down) も隣マスとする
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       ;; pが 0以上で、ボードの要素数よりも小さければ(ボードの範囲なら)
       when (and (>= p 0) (< p *board-hexnum*))
       ;; p を隣のマスとして、リストに加える。
       collect p)))

;;;
;;; 攻撃: 攻撃元のマスはダイスの数が -1 となる
;;;       攻撃先のマスはダイスの数が (1- dice) となる
;;;
;; board -- 盤面
;; player -- プレーヤー(0, 1)
;; src -- 攻撃元のマス(整数)
;; dst -- 攻撃先のマス(整数)
;; dice -- 攻撃元のマスにあるサイコロの数
;; (across -- 配列を走査する場合これを使う)
;; 戻値 -- ダイスが再配置された ボードの配列
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                  for hex across board
                  ;; プレーヤーのマスはダイスの数は1になる (0 1)
                  collect (cond ((eq pos src) (list player 1))
                                ;; 攻撃対象のマスは 1減る (0 (1- dice))
                                ((eq pos dst) (list player (1- dice)))
                                ;; それ以外は何もしない
                                (t hex)))))



;;;
;;; 補給:
;;; 各マスには最大1つしか補給できない 
;;;
;; board -- #((0 1) (1 3) (0 2) (1 1))
;; player -- 0 / 1
;; spare-dice -- 補給できるダイスの数
;; (coerce arr 'list) -- arr をリストに変換する
;;
(defun add-new-dice (board player spare-dice)
  ;; ローカル関数 f
  ;; lst -- boardをリストにしたもの
  ;; n -- 補給できるダイスの数(spare-dice)
  ;; 戻値 -- リスト
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   ;; cur-player -- そのマスの所有者
                   (t (let ((cur-player (caar lst))
                            ;; cur-dice -- そのマスにあるダイスの数
                            (cur-dice (cadar lst)))
                        ;; もしそのマスの所有者が player で、かつ、ダイスの数が max
                        ;; よりも少なければ
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            ;; ダイスの数を +1 して、新しくリストとする。
                            (cons (list cur-player (1+ cur-dice))
                                  ;; 補給できるダイスの数を -1 して再帰
                                  (f (cdr lst) (1- n)))
                            ;; マスの所有者が plalyer でなければなにもせず、
                            ;; そのマスをそのままリストに追加する。
                            (cons (car lst) (f (cdr lst) n))))))))
    ;; ローカル関数の lst に boardをリストにしたものを渡す。
    ;;               n に 補給できるダイスの数(spare-dice) を渡す。
    ;; 戻値のリストを配列に変換して、新しい盤面になる。
    (board-array (f (coerce board 'list) spare-dice))))



(defun play-vs-humen (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-humen (handle-human tree))
      (announce-winner (cadr tree))))


(defun print-info (tree)
  (fresh-line)
  (format t "current-player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))


(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
       for n from 1
       do (let ((action (car move)))
            (fresh-line)
            (format t "~a. " n)
            (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

;; board -- 例: #((0 1) (1 1) (0 2) (1 1))
;; hex -- (0 3) -- 左:所有者  右:サイコロの数
;; player -- 0, 1
(defun winners (board)
  ;; boardのマス目(hex)の所有者のリストを tally とする
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         ;; tally の player の数を数える
         ;; 例: (0 . 2) (1 . 3) <-- consしている
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         ;; tally の重複を削除する
                         (remove-duplicates tally)))
         ;; プレーヤーの数の最大値を best とする
         (best (apply #'max (mapcar #'cdr totals))))
    ;; totals のうち best でないものを削除したリストを造る
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))


(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))


(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; (defparameter *board-size* 3)

;; (defparameter *board-hexnum* (* *board-size* *board-size*))


;; board-attack のテスト
(setq test-board #((0 3) (0 3) (1 3) (1 1)))

;; (setq test-board #((0 1) (1 1) (0 2) (1 1)))

;; (setq test-tree (game-tree test-board 0 2 t))



;; 修正時刻: Mon 2022/04/25 06:27:57
