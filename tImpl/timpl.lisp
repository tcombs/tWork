;written in proofpad
(IN-PACKAGE "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(set-state-ok t)


;input:  xs - a list of numbers

;output: An AVL Tree containing each elemnent in xs
;	    with each element in the tree in the form of a key value
;        pair with x being the key, and nil being the value
;        for each x in xs
(defun num-list->tree(xs)
  (if (consp xs)
      (avl-insert (num-list->tree (cdr xs)) (car xs) nil)
      (empty-tree)))

;input:  cs - a list of characters in the form numeral numeral ... newline numeral numeral ...
;	    where each new line character denotes the start of the next number in the sequence
;        acc - an accumulator that should be nil on the initial call

;output: a list of numbers that the characters represented
(defun chrs->num-list(cs acc)
  (if (consp cs)
      (if (equal #\NewLine (car cs))
          (cons (str->rat (chrs->str acc))
                (chrs->num-list (cdr cs) nil))
          (chrs->num-list (cdr cs) (append acc (list (car cs)))))
      (if (null acc) nil
          (cons (str->rat (chrs->str acc)) nil)))) ;if something in accumulator, convert to number

;input:  x - horizontal coordinate of the grid
;        y - vertical coordinate of the gird
;        w - width of the gird

;output: a unique number key representing the coordinate based on x, y, and w
(defun coord->key(x y w)
  (+ (* y w) x))

;input:  x - horizontal coordinate of the grid
;        y - vertical coordinate of the gird
;        w - width of the gird
;        h - height of the gird

;output: a list of 8 numbers consisting of the 8 adjacent
;        cells to the specified coordinate x,y
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
   ))

;input:  tree - a flat tree, ussually used from a call of (avl-flatten AVLTREE)

;output: a list of numbers that were in the tree
(defun flat-tree->list(tree)
  (if (consp tree)
      (cons (car (car tree)) (flat-tree->list (cdr tree)))
      nil))

;input:  keys - a list of numbers containing all of the neighbors of a cell
;        tree - the current live cell tree

;output: an integer representing the number of cells in keys, that are also in the tree
(defun number-alive(keys tree) 
  (if (consp keys)
      (if (avl-retrieve tree (car keys))
          (+ 1 (number-alive (cdr keys) tree))
          (number-alive (cdr keys) tree))
      0))

;alive-test used on currently alive cells
;input: num-alive - an integer representing the number of alive neighboring cells

;output: t - if the alive cell should remain alive
;        nil - if the alive cell should become dead
(defun alive-test(num-alive)
  (or (= num-alive 2) (= num-alive 3)))

;dead-test used on currently dead cells
;input: num-alive - an integer representing the number of alive neighboring cells  

;output: t - if the dead cell should become alive
;        nil - if the dead cell should remain dead
(defun dead-test(num-alive)
  (= num-alive 3))

;input:  xs - a list of numbers
;        acc-tree - an accumulator tree, should be nil or (empty-tree) on intial call

;output: a set of integers in list form
;        every element in xs, with duplicates removed
(defun list->set (xs acc-tree) 
   (if (consp xs)
       (list->set (cdr xs) (avl-insert acc-tree (car xs) nil))
       (flat-tree->list(avl-flatten acc-tree))))

;input:  list-alive-nodes - a lost of integer representing all of the live nodes in the current state of the game
;        w - width of the gird
;        h - height of the gird
;
;output: a set of nodes representing the nodes the program needs to check
;        the nodes the program needs to check are each of the currently alive nodes,
;        and each of those alive nodes 8 neighbors
(defun generate-nodes-to-check (list-alive-nodes w h)
   (if (consp list-alive-nodes)
	(append (cons (car list-alive-nodes) (get-neighbor-keys (- (car list-alive-nodes) (* (floor (car list-alive-nodes) w) w)) (floor (car list-alive-nodes) w) w h))
   	(generate-nodes-to-check (cdr list-alive-nodes)  w h))
       nil)
)

;input:  old-tree - The old tree containing all of the old live nodes in the game
;        new-tree - an accumulator that will become the next tree, initially nil, or (empty-tree)
;        nodes - a list of numbers containing all of the nodes the program needs to check
;        w - width of the gird
;        h - height of the gird

;output: a new tree containing all of the live nodes in the next state of the
;        game based on the last state of the game.
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

;input:  x - horizontal coordinate of the grid
;        y - vertical coordinate of the gird
;        w - width of the gird
;        h - height of the gird
;        new-tree - a tree with all of the live nodes for the most current state of the game
;        chrs - an accumulator character list containing the pre-text to represeont the game

;output: a string of characters representing the new state of the game
;        where each '#' character is a live node and each '-' character
;        is a dead node
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

;input:  str-pre-text - a string of pretext that represents the state of the game

;output: a string that is ready to be placed into an html file and viewd by a browser
(defun generate-html(str-pre-txt)
   (string-append (string-append "<html><style>pre{line-height: 10px}</style><head><title>game</title></head><body><pre>" str-pre-txt) "</pre></body></html>"))

;input:  xs - a list of integers

;output: a list of strings where each string is
;        the string representation of each value
;        x in xs
(defun ints->strs(xs)
  (if (null xs)
      nil
      (cons (rat->str (car xs) 10) 
            (ints->strs (cdr xs)))))


;input:  input-str - the input string from the game-state.txt file
;                    should be in the form int newline int newline int ....
;                    where the first integer is the width of the grid
;                    and the second integer is the height of the grid
;                    and the remaining integers are all of the live cell in the grid

;output: data in the form (live-cell-strs pre-text)
;        where live-cell-strs is a list of strings of all 
;        of the live cells in the next state of the game
;        and pre-text is the pre-text representation of the
;        new state of the game
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

;i/o function
;input:  html - a string contatinin
;        state - the current ACL2 state
(defun update-display (html state)
   (mv-let (error-wr-display state)
           (string-list->file "game-state.html" html state)
      (if error-wr-display
          (mv error-wr-display html state)
          (mv "display updated" html state))))
;i/o function
;input:  state - the current ACL2 state
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