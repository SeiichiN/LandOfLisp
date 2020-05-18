;;; log.lisp よりこちらへ移動
;;;
;; p105 グラフを可視化する
;;

;; これらは log.lisp からもってきたものそのままである。
(defparameter *wizard-nodes*
  '((living-room
     (you are in the living room.
      a wizard is snoring loudly on the couch.))
    (garden
     (you are in a beautiful garden.
      there is a well in front of you.))
    (attic
     (you are in the attic.
      there is a giant welding torch in the corner.))))


(defparameter *wizard-edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden
     (living-room east door))
    (attic
     (living-room downstairs ladder))))

;;---------------------------------------------
;; graphviz のインストール
;;  $ sudo apt install graphviz
;;
;; test.dot
;; --------------
;;   digraph {
;;       a->b;
;;   }
;; --------------
;;
;; $ neato -Tpng -O test.dot
;;

;; (complement #'alphanumericp) -- 英字あるいは数字である集合の補集合
;; #\_ -- アンダースコアに置き換えている
;; (prin1-to-string exp) -- exp を文字列に置き換えている
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string
                                                   exp)))

(defparameter *max-label-length* 30)

;; ラベルを作成する関数
;; *max-label-length* を超える文字列を短くカットする関数。
;;
;; :pretty -- 画面の幅に応じて改行してくれる
;; concatenate <param1> <param2> ... -- 連結してくれる。
;;            <param1> -- typeを指定。'string だと文字列にしてくれる。
;;            <param2>... -- 連結対象
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3))
                         "...")
            s))
      ""))

;; 出力例
;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;;
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)                  ; 改行
          (princ (dot-name (car node))) ; ノード名(living-room,
                                        ; garden, attic)
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;; edges
;; ((LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; (GARDEN (LIVING-ROOM EAST DOOR)) (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER)))
;; node
;; (LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; edge == cdr node -- (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)
;; (car node) -- living-room 
;; (car edge) -- garden attic
;; (cdr edge) -- west door, upstairs ladder
;;
;; 出力例
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
;;
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

;; 出力例
;; digraph{
;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}
;;
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (let ((filename (concatenate 'string fname ".dot")))
    (with-open-file (*standard-output*
                     filename
                     :direction :output
                     :if-exists :supersede)
      (funcall thunk))
    (ext:shell (concatenate 'string "dot -Tpng -O " filename))))

;;
;; 使用例
;; (graph->png "wizard" *wizard-nodes* *wizard-edges*)
;;
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

;;
;; p117 無向グラフ
;;
;; (
;;  (LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;;  (GARDEN (LIVING-ROOM EAST DOOR))
;;  (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER))
;; )
;;
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

;; 使用例
;; (ugraph->png "wizardB" *wizard-nodes* *wizard-edges*)
;;
(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

;;
;; 出力例
;; > (my-print *wizard-edges*)
;; (GARDEN WEST DOOR) 
;; (ATTIC UPSTAIRS LADDER) 
;; (LIVING-ROOM EAST DOOR) 
;; (LIVING-ROOM DOWNSTAIRS LADDER) 
;;
(defun my-print (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (print edge))
                   (cdar lst)))
           edges))


;;; 修正時刻： Tue May 19 08:38:13 2020
