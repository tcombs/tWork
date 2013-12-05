(IN-PACKAGE "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(set-state-ok t)


(defun num-list->tree(xs)
  (if (consp xs)
      (avl-insert (num-list->tree (cdr xs)) (car xs) nil)
      (empty-tree)))

(defun chrs->num-list(cs acc)
  (if (consp cs)
      (if (equal #\NewLine (car cs))
          (cons (str->rat (chrs->str acc))
                (chrs->num-list (cdr cs) nil))
          (chrs->num-list (cdr cs) (append acc (list (car cs)))))
      (if (null acc) nil
          (cons (str->rat (chrs->str acc)) nil)))) ;if something in accumulator, convert to number

(defun file->chr-list (file-path state)
  (mv-let (str error state)
          (file->string file-path state)
     (if error
         (mv error state)
         (mv (str->chrs str) state))))

(defun coord->key(x y w)
  (+ (* y w) x))

(defun get-neighbor-keys(x y w h)
  (list
   (coord->key (mod (- x 1) w) (mod (- y 1) h) w)
   (coord->key (mod (- x 1) w) y w)
   (coord->key (mod (- x 1) w) (mod (+ y 1) h) w)
   (coord->key x (mod (+ y 1) h) w)
   (coord->key (mod (+ x 1) w) (mod (+ y 1) h) w)
   (coord->key (mod (+ x 1) w) y w)
   (coord->key (mod (+ x 1) w) (mod (- y 1) h) w)
   (coord->key x (mod (- y 1) h) w)
   ;i = y
   ;j = x
   ;(i,j)
   ;(i-1 mod h, j-1 mod w)
   ;(i, j-1 mod w)
   ;(i+1 mod h, j-1 mod w)
   ;(i+1 mod h, j)
   ;(i+1 mod h, j+1 mod w)
   ;(i, j+1 mod w)
   ;(i-1 mod w, j+1 mod w)
   ;(i-1 mod h, j)
   ))

(defun flat-tree->list(tree)
  (if (consp tree)
      (cons (car (car tree)) (flat-tree->list (cdr tree)))
      nil))


(defun number-alive(keys tree) 
  (if (consp keys)
      (if (avl-retrieve tree (car keys))
          (+ 1 (number-alive (cdr keys) tree))
          (number-alive (cdr keys) tree))
      0))

;for the two tests, t -> cell should be alive f-> cell should be dead
(defun alive-test(num-alive)
  (or (= num-alive 2) (= num-alive 3)))

(defun dead-test(num-alive)
  (= num-alive 3))

(defun list->set (xs acc-tree) 
   (if (consp xs)
       (list->set (cdr xs) (avl-insert acc-tree (car xs) nil))
       (flat-tree->list(avl-flatten acc-tree))))

(defun generate-nodes-to-check (list-alive-nodes w h)
   (if (consp list-alive-nodes)
	(append (cons (car list-alive-nodes) (get-neighbor-keys (- (car list-alive-nodes) (* (floor (car list-alive-nodes) w) w)) (floor (car list-alive-nodes) w) w h))
   	(generate-nodes-to-check (cdr list-alive-nodes)  w h))
       nil)
)

(defun get-next-state-tree (old-tree new-tree nodes w h)
   (if (consp nodes)
       (let*
       	(
        		(me (car nodes))
        		(im-alive (avl-retrieve old-tree me))
          	(neighbors (get-neighbor-keys (- me (* (floor me w) w)) (floor me w) w h))
          	(num-neighbors-alive (number-alive neighbors old-tree))
       	)
        (if im-alive
            (if (alive-test num-neighbors-alive)
                (get-next-state-tree old-tree (avl-insert new-tree me nil) (cdr nodes) w h)
                (get-next-state-tree old-tree new-tree (cdr nodes) w h))
            (if (dead-test num-neighbors-alive)
                (get-next-state-tree old-tree (avl-insert new-tree me nil) (cdr nodes) w h)
                (get-next-state-tree old-tree new-tree (cdr nodes) w h))
            )
       )
       new-tree)
)

(defun next-state-tree(w h x y old-tree new-tree chrs)
  (if (= x w)
      (if (= y (- h 1))
          (list new-tree chrs) ;end of matrix
          (next-state-tree w h 0 (+ y 1) old-tree new-tree (append chrs (list #\NewLine)))) ;end of row, start new row
      (let* (
             (me (coord->key x y w))
             (im-alive (avl-retrieve old-tree me))
             (neighbors (get-neighbor-keys x y w h))
             (num-neighbors-alive (number-alive neighbors old-tree))
             ) 
        (if im-alive
            (if (alive-test num-neighbors-alive)
                (next-state-tree w h (+ x 1) y old-tree (avl-insert new-tree me nil) (append chrs (list #\#))) ;alive
                (next-state-tree w h (+ x 1) y old-tree new-tree (append chrs (list #\-)))) ;dead
            (if (dead-test num-neighbors-alive)
                (next-state-tree w h (+ x 1) y old-tree (avl-insert new-tree me nil) (append chrs (list #\#))) ;alive
                (next-state-tree w h (+ x 1) y old-tree new-tree (append chrs (list #\-)))))))) ;dead

(defun generate-pre-text (x y w h new-tree chrs)
    (if (= x w)
        (if (= y (- h 1))
            (chrs->str chrs) ;end of matrix
            (generate-pre-text 0 (+ y 1) w h new-tree (append chrs (list #\NewLine))) ;end of row, move to next row
            )
            (if (avl-retrieve new-tree (coord->key x y w))
                (generate-pre-text (+ x 1) y w h new-tree (append chrs (list #\#))) ;im alive
                (generate-pre-text (+ x 1) y w h new-tree (append chrs (list #\-))))   ; in the middle of matrix
))

(defun generate-html(str-pre-txt)
   (string-append (string-append "<html><style>pre{line-height: 10px}</style><head><title>game</title></head><body><pre>" str-pre-txt) "</pre></body></html>"))


(defun ints->strs(xs)
  (if (null xs)
      nil
      (cons (rat->str (car xs) 10) 
            (ints->strs (cdr xs)))))
      

(defun write-game-state (str-list-game-state state) (mv-let (error state)
          (string-list->file "game-state"
                             str-list-game-state
                                    state)
     (if error
         (mv error state)
         (mv t state))))

(defun input-str->output-strs(input-str)
   (let* (
         (old-num-list (chrs->num-list (str->chrs input-str) nil))
         (w (car old-num-list))
         (h (car (cdr old-num-list)))
         (old-tree (num-list->tree (cdr (cdr old-num-list))))
         (nodes-to-check (list->set (generate-nodes-to-check (flat-tree->list (avl-flatten old-tree)) w h) nil))
         (new-tree (get-next-state-tree old-tree (empty-tree) nodes-to-check w h))
         (pre-text (generate-pre-text 0 0 w h new-tree nil))
         )
     (list (ints->strs (cons w (cons h (flat-tree->list (avl-flatten new-tree))))) pre-text)))



(defun update-display (html state)
   (mv-let (error-wr-display state)
           (string-list->file "game-state.html" html state)
      (if error-wr-display
          (mv error-wr-display html state)
          (mv "display updated" html state))))

(defun main (state)
  (mv-let (in-string err state)                   ; retrieve state of world
	     (file->string "game-state.txt" state)
         (let* (
  			 (data (input-str->output-strs in-string)) ; update world
                (new-state (car data))
      		 (new-html (list (generate-html (cadr data))))
      		)
               (mv-let (error-wr-nw state)
                       (string-list->file "game-state.txt" new-state state)
                  (if error-wr-nw
                      (mv error-wr-nw err state)        ; update display
                      (update-display new-html state))))))

