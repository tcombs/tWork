(in-package "ACL2")
(include-book "tImpl")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)

(defun nat-listp (xs)
  (if (= 1 (len xs))
      (natp (car xs))
      (if (natp (car xs))
          (nat-listp (cdr xs))
          nil)))
(defthm list->set-decreases-length-thm
   (implies (and (consp xs) (nat-listp xs)) (<= (len xs)) (len (list->set xs nil))))

(defproperty list->set-decreases-length-prop
   (xs :value (random-list-of (random-natural)))
   (<= (len xs) (len (list->set xs nil))))