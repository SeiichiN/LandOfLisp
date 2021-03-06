

(defparameter *nodes* '((living-room
                          (you are in the living room.
                           a wizard is snoring loudly on the couch.))
                        (garden
                          (you are in a beautiful garden.
                           there is a well in front of you.))
                        (attic
                          (you are in the attic.
                           there is a giant welding torch in the corner.))))


(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)(attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))


(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

;; loc の中に objs があるかどうかを調べて(at-loc-p)
;; あるもののリストを返す
;; @params:
:;   loc -- 現在いる場所 living-room / garden / attic
;;   objs -- オブジェクトのリスト *objects* -- '(whiskey bucket chain frog)
;;   obj-locs -- *object-locations*
;; @return:
;;   現在いる場所にあるオブジェクトのリスト
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; 現在いる場所で見えるオブジェクトを描写する
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs
                                                       obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; direction -- west / east / upstair / downstair
(defun walk (direction)
  ;; next -- nextには例えば (garden west door) というリストが入る
  ;;         無ければ nil
  (let ((next (find direction
                    ;; その場所からの通り道のリスト
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))      ; keyをcadrにもつ要素
    (if next
        ;; car next -- living-room / garden / attic
        (progn (setf *location* (car next))
               (look))
        ;; next が nil ならば
        '(you cannot go that way.))))

;; オブジェクトを手に取る
;; *object-locations*の先頭に (object body) を付け加える
(defun pickup (object)
  (cond ((member object
                 ;; 現在いる場所にあるオブジェクトのリスト
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


(defun game-repl ()
    (let ((cmd (game-read)))
       (unless (eq (car cmd) 'quit)
          (game-print (game-eval cmd))
          (game-repl))))


(defun game-read ()
   (let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")"))))
      (flet ((quote-it (x)
               (list 'quote x)))
         (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


(defun game-print (lst)
   (princ (coerce (tweak-text (coerce (string-trim "() "
                                                   (prin1-to-string lst))
                                      'list)
                              t
                              nil)
                  'string))
   (fresh-line))

;;;================================================
;;; 7章 単純なリストの先へ
;;;
;; p103 連想リスト
;;
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))

;; p104 木構造のデータの可視化
;;
(defparameter *house* '((walls
                         (mortar
                          (cement)
                          (water)
                          (sand))
                         (bricks))
                        (windows
                         (glass)
                         (frame)
                         (curtains))
                        (roof
                         (singles)
                         (chimney))))

;; p105 グラフを可視化する
;;
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
;; 
;;   digraph {
;;       a->b;
;;   }
;;
;; neato -Tpng -O test.dot
;;




;; 修正時刻： Mon May 18 16:59:30 2020
