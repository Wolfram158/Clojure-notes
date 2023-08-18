(defn binarySearchLeft [arr key left right] (let [mid (bit-shift-right (+ left right) 1)]
                                              (cond
                                                (> (- right left) 1)
                                                (cond
                                                  (< (get arr mid) key)
                                                  (recur arr key mid right)
                                                  :else
                                                  (recur arr key left mid))
                                                :else
                                                (cond
                                                  (= (get arr left) key)
                                                  left
                                                  (= (get arr right) key)
                                                  right
                                                  :else
                                                  -1))))

(defn binarySearchRight [arr key left right] (let [mid (bit-shift-right (+ left right) 1)]
                                               (cond
                                                 (> (- right left) 1)
                                                 (cond
                                                   (<= (get arr mid) key)
                                                   (recur arr key mid right)
                                                   :else
                                                   (recur arr key left mid))
                                                 :else
                                                 (cond
                                                   (= (get arr right) key)
                                                   right
                                                   (= (get arr left) key)
                                                   left
                                                   :else
                                                   -1))))

(defn count-pairs-impl [sorted num index] (let [left (binarySearchLeft sorted (- num (get sorted index)) 0 (dec index))
                                                right (binarySearchRight sorted (- num (get sorted index)) 0 (dec index))]
                                            (if (not= left -1) (inc (- right left)) 0)))

(defn count-pairs [arr num] (let [sorted (vec (sort arr))]
                              (apply +
                                     (for [i (range 0 (count sorted))]
                                       (count-pairs-impl sorted num i)))))
