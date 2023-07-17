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

(defn count-pairs [arr num] (let [sorted (vec (sort arr)) answer (atom 0) index (atom 0) left (atom 0) right (atom 0)]
                              (do
                                (while (< @index (count arr))
                                  (do
                                    (reset! left (binarySearchLeft sorted (- num (get sorted @index)) 0 (dec @index)))
                                    (if
                                      (not= @left -1)
                                      (do
                                        (reset! right (binarySearchRight sorted (- num (get sorted @index)) 0 (dec @index)))
                                        (swap! answer + (inc (- @right @left)))))
                                    (swap! index inc)))
                                @answer)))
