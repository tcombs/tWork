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

(defun generate-html(str-pre-txt state)
  (mv-let (error state)
          (string-list->file "input.html"
              (list "<html><body><pre>" str-pre-txt
                    "</pre></body></html>")
                                    state)
     (if error
         (mv error state)
         (mv t state))))

(defun flat-tree->list(tree)
  (if (consp tree)
      (cons (car (car tree)) (flat-tree->list (cdr tree)))
      nil))

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
         (old-tree (num-list->tree(cdr (cdr old-num-list))))
         (tree-chars (next-state-tree w h 0 0 old-tree (empty-tree) nil))
         (new-tree (car tree-chars))
         )
     (ints->strs (cons w (cons h (flat-tree->list (avl-flatten new-tree)))))))

;(defun main (state)
;  (mv-let (input-as-string error-open state)
;          (file->string "game-state" state)
;     (if error-open
;         (mv error-open state)
;         (mv-let (error-close state)
;                 (string-list->file "game-state"
;                                    (input-str->output-strs input-as-string)
;                                    state)
;            (if error-close
;                (mv error-close state)
;                (mv (string-append "input file: "
;                     (string-append "game-state"
;                      (string-append ", output file: " "game-state")))
;                    state))))))

(defun main (state)
  (mv-let (input-as-string error-open state)
          (file->string "game-state.txt" state)
     (if error-open
         (mv error-open state)
         (mv-let (error-close state)
                 (string-list->file "game-state.txt"
                                    (input-str->output-strs input-as-string)
                                    state)
            (if error-close
                (mv error-close state)
                (mv (input-str->output-strs input-as-string)
                    state))))))