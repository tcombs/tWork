(in-package "ACL2")
(include-book "tImpl")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

(defun natural-listp (xs)
  (if (= 1 (len xs))
      (natp (car xs))
      (if (natp (car xs))
          (natural-listp (cdr xs))
          nil)))
(defthm always-eight-neighbors-thm
   (implies (and (natp x) (natp y) (natp w) (natp h))
            (= (len (get-neighbor-keys x y w h)) 8)))

(defproperty always-eight-neighbors-prop
   (x :value (random-natural)
    y :value (random-natural)
    w :value (random-natural)
    h :value (random-natural))
   (implies (and (natp x) (natp y) (natp w) (natp h))
            (= (len (get-neighbor-keys x y w h)) 8)))

(defthm coord->key-delivers-natural-number-thm
   (implies (and (natp x) (natp y) (natp w)) (natp (coord->key x y w))))

(defproperty coord->key-delivers-natural-number-prop
   (x :value (random-natural)
    y :value (random-natural)
    w :value (random-natural))
   (implies (and (natp x) (natp y) (natp w)) (natp (coord->key x y w))))

(defproperty round-trip-trees-preserve-length-prop
   (xs :value (random-list-of (random-natural)))
   (equal (len xs) (len (flat-tree->list (avl-flatten(num-list->tree xs))))))

(defproperty num-list->tree-is-a-tree-prop
  (xs :value (random-list-of (random-natural)))
  (avl-tree? (num-list->tree xs)))