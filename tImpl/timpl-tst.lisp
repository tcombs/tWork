(in-package "ACL2")
(include-book "tImpl")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

;num-list->tree
(check-expect (num-list->tree nil) nil)

(check-expect (num-list->tree 0) nil)

(check-expect (num-list->tree(list 1 2)) '(2 2 NIL (1 1 NIL NIL NIL) NIL))

(check-expect (num-list->tree(list 2 3 4 5)) '(3 4 NIL (2 3 NIL (1 2 NIL NIL NIL) NIL) (1 5 NIL NIL NIL)))


;chrs->num-list
(check-expect (chrs->num-list nil nil) nil)

(check-expect (chrs->num-list '( #\NewLine) nil) '(0))

(check-expect (chrs->num-list '( #\1 #\NewLine #\3) nil) '(1 3))

(check-expect (chrs->num-list '( #\NewLine #\NewLine) nil) '(0 0))


;coord->key
(check-expect (coord->key 0 0 0) 0)

(check-expect (coord->key 1 1 1) 2)

(check-expect (coord->key 1 2 3) 7)

(check-expect (coord->key 5 5 7) 40)


;get-neighbor-keys
(check-expect (get-neighbor-keys 0 0 1 1) '(0 0 0 0 0 0 0 0))

(check-expect (get-neighbor-keys 0 0 5 5) '(24 4 9 5 6 1 21 20))

(check-expect (get-neighbor-keys 2 2 5 5) '(6 11 16 17 18 13 8 7))

(check-expect (get-neighbor-keys 25 36 100 100) '(3524 3624 3724 3725 3726 3626 3526 3525))


;flat-tree->list
(check-expect (flat-tree->list nil) nil)

(check-expect (flat-tree->list (avl-flatten (avl-insert nil nil nil))) '(nil))

(check-expect (flat-tree->list (avl-flatten (avl-insert nil 6 nil))) '(6))

(check-expect (flat-tree->list (avl-flatten (avl-insert(avl-insert (avl-insert nil 4 nil)  5 nil) 6 nil))) '(4 5 6))


;number-alive
(check-expect (number-alive nil nil) 0 )

(check-expect(number-alive nil (avl-insert(avl-insert nil 5 nil) 6 nil)) 0 )

(check-expect(number-alive '()(avl-insert(avl-insert nil 5 nil) 6 nil)) 0 )

(check-expect (number-alive '(5 6 7)(avl-insert(avl-insert nil 5 nil) 6 nil)) 2)


;alive-test
(check-expect (alive-test 0) nil)

(check-expect (alive-test 2) T)

(check-expect (alive-test 3) T)

(check-expect (alive-test 12) nil)


;dead-test
(check-expect (dead-test 0) nil)

(check-expect (dead-test 2) nil)

(check-expect (dead-test 3) T)

(check-expect (dead-test 12) nil)


;list->set
(check-expect (list->set nil nil) nil)

(check-expect (list->set '(1 2 3) nil) '(1 2 3))

(check-expect (list->set '(1  2 2 2 2 2 3) nil) '(1 2 3))

(check-expect (list->set '(1 1 1 1 1 1 1 1 1) nil) '(1))

;generate-nodes-to-check
(check-expect (generate-nodes-to-check nil nil nil) nil)

(check-expect (generate-nodes-to-check nil 5 5) nil)

(check-expect (generate-nodes-to-check '(0) 2 2) '(0 3 1 3 2 3 1 3 2))

(check-expect (generate-nodes-to-check '(8) 5 5) '(8 2 7 12 13 14 9 4 3))

;get-next-state-tree
(check-expect (get-next-state-tree nil nil nil nil nil) nil)



;generate-pre-text
(check-expect (str->chrs(generate-pre-text 5 5 5 5 (avl-insert(avl-insert nil 2 nil) 3 nil) nil)) nil)

(check-expect (str->chrs(generate-pre-text 0 0 5 5 nil nil)) '(#\- #\- #\- #\- #\- #\Newline
                             #\- #\- #\- #\- #\- #\Newline #\-
                          #\- #\- #\- #\- #\Newline #\- #\- #\-
                                #\- #\- #\Newline #\- #\- #\- #\- #\-))


(check-expect (str->chrs(generate-pre-text 0 0 5 5 (avl-insert(avl-insert nil 5 nil) 6 nil) nil))  '(#\- #\- #\- #\- #\- #\Newline
                                            #\# #\# #\- #\- #\- #\Newline #\-
                                          #\- #\- #\- #\- #\Newline #\- #\- #\-
                                             #\- #\- #\Newline #\- #\- #\- #\- #\-))

(check-expect (str->chrs(generate-pre-text 1 2 5 5 (avl-insert(avl-insert nil 2 nil) 3 nil) nil)) '(#\- #\- #\- #\- #\Newline #\- #\- #\-
                                          #\- #\- #\Newline #\- #\- #\- #\- #\-))



;generate-html
(check-expect (str->chrs(generate-html "test")) '(#\< #\h #\t #\m #\l #\> #\< #\s #\t #\y #\l
     #\e #\> #\p #\r #\e #\{ #\l #\i #\n #\e
     #\- #\h #\e #\i #\g #\h #\t #\: #\Space
     #\1 #\0 #\p #\x #\} #\< #\/ #\s #\t #\y
     #\l #\e #\> #\< #\h #\e #\a #\d #\> #\<
     #\t #\i #\t #\l #\e #\> #\g #\a #\m #\e
     #\< #\/ #\t #\i #\t #\l #\e #\> #\< #\/
     #\h #\e #\a #\d #\> #\< #\b #\o #\d #\y
     #\> #\< #\p #\r #\e #\> #\t #\e #\s #\t
     #\< #\/ #\p #\r #\e #\> #\< #\/ #\b #\o
     #\d #\y #\> #\< #\/ #\h #\t #\m #\l #\>)
              )

(check-expect (str->chrs(generate-html "----- ----- ----- ----- -----")) '(#\< #\h #\t #\m #\l #\> #\< #\s #\t #\y #\l
     #\e #\> #\p #\r #\e #\{ #\l #\i #\n #\e
     #\- #\h #\e #\i #\g #\h #\t #\: #\Space
     #\1 #\0 #\p #\x #\} #\< #\/ #\s #\t #\y
     #\l #\e #\> #\< #\h #\e #\a #\d #\> #\<
     #\t #\i #\t #\l #\e #\> #\g #\a #\m #\e
     #\< #\/ #\t #\i #\t #\l #\e #\> #\< #\/
     #\h #\e #\a #\d #\> #\< #\b #\o #\d #\y
     #\> #\< #\p #\r #\e #\> #\- #\- #\- #\-
     #\- #\Space #\- #\- #\- #\- #\- #\Space
     #\- #\- #\- #\- #\- #\Space #\- #\-
     #\- #\- #\- #\Space #\- #\- #\- #\- #\-
     #\< #\/ #\p #\r #\e #\> #\< #\/ #\b #\o
     #\d #\y #\> #\< #\/ #\h #\t #\m #\l #\>)
              )

(check-expect (str->chrs(generate-html "----- ##--- ##### ----- -----")) '(#\< #\h #\t #\m #\l #\> #\< #\s #\t #\y #\l
     #\e #\> #\p #\r #\e #\{ #\l #\i #\n #\e
     #\- #\h #\e #\i #\g #\h #\t #\: #\Space
     #\1 #\0 #\p #\x #\} #\< #\/ #\s #\t #\y
     #\l #\e #\> #\< #\h #\e #\a #\d #\> #\<
     #\t #\i #\t #\l #\e #\> #\g #\a #\m #\e
     #\< #\/ #\t #\i #\t #\l #\e #\> #\< #\/
     #\h #\e #\a #\d #\> #\< #\b #\o #\d #\y
     #\> #\< #\p #\r #\e #\> #\- #\- #\- #\-
     #\- #\Space #\# #\# #\- #\- #\- #\Space
     #\# #\# #\# #\# #\# #\Space #\- #\-
     #\- #\- #\- #\Space #\- #\- #\- #\- #\-
     #\< #\/ #\p #\r #\e #\> #\< #\/ #\b #\o
     #\d #\y #\> #\< #\/ #\h #\t #\m #\l #\>))

(check-expect (str->chrs(generate-html "##### ##### ##### ##### #####")) '(#\< #\h #\t #\m #\l #\> #\< #\s #\t #\y #\l
     #\e #\> #\p #\r #\e #\{ #\l #\i #\n #\e
     #\- #\h #\e #\i #\g #\h #\t #\: #\Space
     #\1 #\0 #\p #\x #\} #\< #\/ #\s #\t #\y
     #\l #\e #\> #\< #\h #\e #\a #\d #\> #\<
     #\t #\i #\t #\l #\e #\> #\g #\a #\m #\e
     #\< #\/ #\t #\i #\t #\l #\e #\> #\< #\/
     #\h #\e #\a #\d #\> #\< #\b #\o #\d #\y
     #\> #\< #\p #\r #\e #\> #\# #\# #\# #\#
     #\# #\Space #\# #\# #\# #\# #\# #\Space
     #\# #\# #\# #\# #\# #\Space #\# #\#
     #\# #\# #\# #\Space #\# #\# #\# #\# #\#
     #\< #\/ #\p #\r #\e #\> #\< #\/ #\b #\o
     #\d #\y #\> #\< #\/ #\h #\t #\m #\l #\>)
              )

;ints->strs
(check-expect (ints->strs nil) nil)

(check-expect (ints->strs (ints->strs '(0))) '("0"))

(check-expect (ints->strs (ints->strs '(0 0 0 0 0 ))) '("0" "0" "0" "0" "0"))

(check-expect (ints->strs '(0 1 2 3)) '("0" "1.0000000000" "2.0000000000" "3.0000000000"))












