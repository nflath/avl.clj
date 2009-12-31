;; avl.clj
;;
;; This package provides a semi-optimized AVL tree. AVL Trees are
;; represented as structure that have four 'elements': the data, left
;; child, right child, and height.
;;
;; Methods Exported:
;; (avl-insert tree val < >)  - Inserts into the AVL tree using the given < and > operators.
;; (avl-insert tree val)      - Inserts into the AVL tree using the default < and > operators
;; (avl-lookup tree val < >)  - Returns true if val is in the AVL tree using the given < and > operators.
;; (avl-loookup tree val)     - Returns true if val is in the AVL tree using the default < and > operators.
;; (avl-remove tree val < >)  - Removes from the AVL tree using the given < and > operators.
;; (avl-remove tree val)      - Removes from the AVL tree using the default < and > operators.
;;
;; Written by: Nathaniel Flath

(ns structures.avl (:import (java.util Random)))

(defstruct avl-tree :data :height :left :right)

(defn get-height
  "Returns the height of an AVL tree, or -1 if nil"
  ([tree] (if tree (tree :height) -1)))

(defn tree-assoc
  "Helper function that will create a tree of the correct form and height"
  ([tree left right]
     (assoc tree
       :left left
       :right right
       :height (inc (max (get-height left) (get-height right))))))

(defn balance-factor
  "Returns the height of the right subtree - height of left subtree"
  ([tree] (- (get-height (tree :right)) (get-height (tree :left)))))

(defn rrotate [tree]
  "Performs a right rotation on an AVL tree"
  (let [right-height (inc (max (get-height (tree :right)) (get-height ((tree :left) :right))))]
    (assoc (tree :left)
      :height (inc (max right-height (get-height ((tree :left) :left))))
      :right (assoc tree
               :height right-height
               :left (if (tree :left) ((tree :left) :right) nil)))))

(defn lrotate [tree]
  "Performs a left rotation on an AVL tree"
  (let [left-height (inc (max (get-height (tree :left)) (get-height ((tree :right) :left))))]
    (assoc (tree :right)
      :height (inc (max left-height (get-height ((tree :right) :right))))
      :left (assoc tree
              :height left-height
              :right (if (tree :right) ((tree :right) :left) nil)))))

(defn balance [tree]
  "Return a balanced version of the AVL tree"
  (if (and tree (< (Math/abs (balance-factor tree)) 2)) tree
      (if tree
        (cond
         (= (balance-factor tree) 2) (if (>= (balance-factor (tree :right)) 0)
                                       (lrotate tree)
                                       (lrotate (assoc tree :right (rrotate (tree :right)))))
         (= (balance-factor tree) -2) (if (<= (balance-factor (tree :left)) 0)
                                        (rrotate tree)
                                        (rrotate (assoc tree :left (lrotate (tree :left)))))))))

(defn predecessor [tree]
  "Returns the predecessor of the root node of tree"
  (#(if % (if (% :right) (recur (% :right)) %)) (tree :left)))

(defn avl-lookup
  "Returns the data from the tree corresponding to val if it exists, otherwise nil"
  ([tree val < >]
     (if tree
       (cond
        (< (tree :data) val) (tree-lookup (tree :right) val)
        (> (tree :data) val) (tree-lookup (tree :left) val)
        true true)
       nil))
  ([tree val] (tree-lookup tree val < >)))

(defn avl-insert
  "Inserts a new node into an AVL tree"p
  ([tree val < >]
     (if tree
       (balance
        (cond
         (> (tree :data) val) (tree-assoc tree (avl-insert (tree :left) val < >) (tree :right))
         (< (tree :data) val) (tree-assoc tree (tree :left) (avl-insert (tree :right) val < >))
         true (assoc tree :data val)))
       (struct avl-tree val 0)))
  ([tree val] (avl-insert tree val < >)))

(defn avl-remove
  "Removes a node from an AVL tree"
  ([tree val < >]
     (if tree
       (balance
        (cond
         (< (tree :data) val) (tree-assoc tree (tree :left) (avl-remove (tree :right) val))
         (> (tree :data) val) (tree-assoc tree (avl-remove (tree :left) val) (tree :right))
         true (if (not (= (tree :height) 0))
                (let [new-tree
                      (if (predecessor tree)
                        (assoc (tree-assoc tree
                                           (avl-remove (tree :left) ((predecessor tree) :data))
                                           (tree :right))
                          :data ((predecessor tree) :data))
                        (tree :right))]
                  new-tree))))))
  ([tree val] (avl-remove tree val < >)))

(defn assert-correct-heights [tree]
  "Test method - takes a tree, and asserts it is properly balanced and
   the heights are correct for each node."
  (if tree
    (do
      (try
       (assert (< (balance-factor tree) 2))
       (assert (.equals (tree :height) (inc (max
                                             (assert-correct-heights (tree :left))
                                             (assert-correct-heights (tree :right))))))
       (catch java.lang.Exception e
         (do
           (throw e))))
      (tree :height))
    -1))

(defn test-all []
  "Tests the AVL tree"
  (dotimes [x 100]
    (def tree (ref (avl-insert nil 50)))
    (let [rand (Random.)]
      (dotimes [x 100]
        (dosync
         (alter tree avl-insert (.nextInt rand 1000))))
      (assert-correct-heights tree)
      (def tree (deref tree))
      (dotimes [x 1000]
        (let [t (avl-remove tree x)]
          (assert-correct-heights t)
          (assert (not (avl-lookup t x)))))
      (def tree (ref tree))
      (dotimes [x 1000]
        (dosync
         (alter tree avl-remove x)
         (assert (not (avl-lookup (deref tree) x)))
         (assert-correct-heights (deref tree)))))))