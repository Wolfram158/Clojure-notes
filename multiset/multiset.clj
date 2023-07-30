(defn add [multiset] (fn [e] (let [signal (atom true) i (atom 0) result (atom [])]
                   (cond (empty? (get multiset :data))
                         (conj (get multiset :data) [e 1])
                         :else
                         (do
                           (while (< @i (count (get multiset :data)))
                             (do
                               (cond (< (hash e) (hash (first (get (get multiset :data) @i))))
                                     (do
                                       (reset! result
                                               (vec (concat (subvec (get multiset :data) 0 @i)
                                                            [[e 1]]
                                                            (subvec (get multiset :data) @i))))
                                       (reset! signal false)
                                       (reset! i (count (get multiset :data))))
                                     (java.util.Objects/equals e (first (get (get multiset :data) @i)))
                                     (do
                                       (reset!
                                         result
                                         (vec (concat (subvec (get multiset :data) 0 @i)
                                                      [[e (inc (second (get (get multiset :data) @i)))]]
                                                      (subvec (get multiset :data) (inc @i)))))
                                       (reset! signal false)
                                       (reset! i (count (get multiset :data))))
                                     )
                               (swap! i inc)
                               ))
                           (if (= true @signal)
                             (reset! result (conj (get multiset :data) [e 1])))
                           @result
                           )
                         )
                   )))

(defn add-advanced [multiset] (fn [e amount] (let [signal (atom true) i (atom 0) result (atom [])]
                               (cond (empty? (get multiset :data))
                                     (conj (get multiset :data) [e amount])
                                     :else
                                     (do
                                       (while (< @i (count (get multiset :data)))
                                         (do
                                           (cond (< (hash e) (hash (first (get (get multiset :data) @i))))
                                                 (do
                                                   (reset!
                                                     result
                                                     (vec (concat (subvec (get multiset :data) 0 @i)
                                                                  [[e amount]]
                                                                  (subvec (get multiset :data) @i))))
                                                   (reset! signal false)
                                                   (reset! i (count (get multiset :data))))
                                                 (java.util.Objects/equals e (first (get (get multiset :data) @i)))
                                                 (do
                                                   (reset!
                                                     result
                                                     (vec (concat (subvec (get multiset :data) 0 @i)
                                                                  [[e (+ amount (second (get (get multiset :data) @i)))]]
                                                                  (subvec (get multiset :data) (inc @i)))))
                                                   (reset! signal false)
                                                   (reset! i (count (get multiset :data))))
                                                 )
                                           (swap! i inc)
                                           ))
                                       (if (= true @signal)
                                         (reset! result (conj (get multiset :data) [e amount])))
                                       @result
                                       )
                                     )
                               )))

(defn vec-remove [pos coll] (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn remove [multiset] (fn [e] (let [i (atom 0) result (atom (get multiset :data))]
                                  (do
                                    (while (< @i (count (get multiset :data)))
                                      (do
                                        (if (java.util.Objects/equals e (first (get (get multiset :data) @i)))
                                          (cond
                                            (= (second (get (get multiset :data) @i)) 1)
                                            (reset! result (vec-remove @i (get multiset :data)))
                                            :else
                                            (reset!
                                              result
                                              (assoc (get multiset :data) @i [e (dec (second (get (get multiset :data) @i)))]))))
                                        (swap! i inc)))
                                    @result))))

(defn intersect [multisetA multisetB] (let [pointerA (atom 0) pointerB (atom 0) result (atom {:data []})]
                                        (do
                                          (while (and
                                              (< @pointerA (count (get multisetA :data)))
                                              (< @pointerB (count (get multisetB :data))))
                                            (cond
                                              (<
                                                (hash (first (get (get multisetA :data) @pointerA)))
                                                (hash (first (get (get multisetB :data) @pointerB))))
                                              (swap! pointerA inc)
                                              (>
                                                (hash (first (get (get multisetA :data) @pointerA)))
                                                (hash (first (get (get multisetB :data) @pointerB))))
                                              (swap! pointerB inc)
                                              :else
                                              (do
                                                (swap! result #(assoc % :data ((add-advanced %)
                                                                               (first (get (get multisetA :data) @pointerA))
                                                                               (Math/min
                                                                                 (second (get (get multisetA :data) @pointerA))
                                                                                 (second (get (get multisetB :data) @pointerB))))))
                                                (swap! pointerA inc)
                                                (swap! pointerB inc))
                                              ))
                                          @result)))

(defn union [multisetA multisetB] (let [pointerA (atom 0) pointerB (atom 0) result (atom {:data []})]
                                        (do
                                          (while (and
                                                   (< @pointerA (count (get multisetA :data)))
                                                   (< @pointerB (count (get multisetB :data))))
                                            (cond
                                              (<
                                                (hash (first (get (get multisetA :data) @pointerA)))
                                                (hash (first (get (get multisetB :data) @pointerB))))
                                              (do
                                                (swap! result #(assoc % :data ((add-advanced %)
                                                                               (first (get (get multisetA :data) @pointerA))
                                                                               (second (get (get multisetA :data) @pointerA)))))
                                                (swap! pointerA inc))
                                              (>
                                                (hash (first (get (get multisetA :data) @pointerA)))
                                                (hash (first (get (get multisetB :data) @pointerB))))
                                              (do
                                                (swap! result #(assoc % :data ((add-advanced %)
                                                                               (first (get (get multisetB :data) @pointerB))
                                                                               (second (get (get multisetB :data) @pointerB)))))
                                                (swap! pointerB inc))
                                              :else
                                              (do
                                                (swap! result #(assoc % :data ((add-advanced %)
                                                                               (first (get (get multisetA :data) @pointerA))
                                                                               (Math/max
                                                                                 (second (get (get multisetA :data) @pointerA))
                                                                                 (second (get (get multisetB :data) @pointerB))))))
                                                (swap! pointerA inc)
                                                (swap! pointerB inc))
                                              ))
                                          (while (< @pointerA (count (get multisetA :data)))
                                            (do
                                              (swap! result #(assoc % :data ((add-advanced %)
                                                                             (first (get (get multisetA :data) @pointerA))
                                                                             (second (get (get multisetA :data) @pointerA)))))
                                              (swap! pointerA inc)))
                                          (while (< @pointerB (count (get multisetB :data)))
                                            (do
                                              (swap! result #(assoc % :data ((add-advanced %)
                                                                             (first (get (get multisetB :data) @pointerB))
                                                                             (second (get (get multisetB :data) @pointerB)))))
                                              (swap! pointerB inc)))
                                          @result)))

(defn toString [multiset] (str "Bag[" (clojure.string/join ", " (mapv #(str (first %) ":" (second %)) (get multiset :data))) "]"))

(def multiset {:data []})

(def set1 multiset)
(def set1 (assoc set1 :data ((add set1) 2)))
(def set1 (assoc set1 :data ((add set1) 2)))
(def set1 (assoc set1 :data ((add set1) 2)))
(def set1 (assoc set1 :data ((add set1) 3)))
(def set1 (assoc set1 :data ((add set1) "a")))
(def set1 (assoc set1 :data ((add set1) "b")))
(def set1 (assoc set1 :data ((add set1) "cde")))
(def set1 (assoc set1 :data ((add set1) "cde")))
(toString set1)

(def set2 multiset)
(def set2 (assoc set2 :data ((add set2) 2)))
(def set2 (assoc set2 :data ((add set2) 2)))
(def set2 (assoc set2 :data ((add set2) 3)))
(def set2 (assoc set2 :data ((add set2) 4)))
(def set2 (assoc set2 :data ((add set2) "a")))
(def set2 (assoc set2 :data ((add set2) "c")))
(def set2 (assoc set2 :data ((add set2) "cdef")))
(def set2 (assoc set2 :data ((add set2) "cdef")))
(toString set2)

(def set3 (union set1 set2))
(toString set3)

(def set4 (intersect set1 set2))
(toString set4)

(def set1 (assoc set1 :data ((remove set1) "cde")))
(toString set1)
(def set1 (assoc set1 :data ((remove set1) "cde")))
(toString set2)

(def set2 (assoc set2 :data ((remove set2) "ccc")))
(toString set2)
