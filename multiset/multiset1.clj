(defprotocol IGetData (get-data [this]))
(defprotocol ISetData (set-data [this o]))

(deftype Multiset [^:unsynchronized-mutable data]
    IGetData
    (get-data [this] data)
    ISetData
    (set-data [this o] (set! data o))
    Object
    (toString [_]
        (str "Bag[" (clojure.string/join ", " (mapv #(str (first %) ":" (second %)) data)) "]")))

(defmulti add class)
(defmethod add Multiset [this]
    (fn [e] (let [signal (atom true) i (atom 0) result (atom [])]
                   (cond (empty? (.get-data this))
                         (.set-data this [[e 1]])
                         :else
                         (do
                           (while (< @i (count (.get-data this)))
                             (do
                               (cond (< (hash e) (hash (first (get (.get-data this) @i))))
                                     (do
                                       (reset! result
                                               (vec (concat (subvec (.get-data this) 0 @i)
                                                            [[e 1]]
                                                            (subvec (.get-data this) @i))))
                                       (reset! signal false)
                                       (reset! i (count (.get-data this))))
                                     (java.util.Objects/equals e (first (get (.get-data this) @i)))
                                     (do
                                       (reset!
                                         result
                                         (vec (concat (subvec (.get-data this) 0 @i)
                                                      [[e (inc (second (get (.get-data this) @i)))]]
                                                      (subvec (.get-data this) (inc @i)))))
                                       (reset! signal false)
                                       (reset! i (count (.get-data this))))
                                     )
                               (swap! i inc)
                               ))
                           (if (= true @signal)
                             (reset! result (conj (.get-data this) [e 1])))
                           (.set-data this @result)
                           )
                         ))))

(defmulti add-advanced class)
(defmethod add-advanced Multiset [this]
    (fn [e amount] (let [signal (atom true) i (atom 0) result (atom [])]
                               (cond (empty? (.get-data this))
                                     (.set-data this [[e amount]])
                                     :else
                                     (do
                                       (while (< @i (count (.get-data this)))
                                         (do
                                           (cond (< (hash e) (hash (first (get (.get-data this) @i))))
                                                 (do
                                                   (reset!
                                                     result
                                                     (vec (concat (subvec (.get-data this) 0 @i)
                                                                  [[e amount]]
                                                                  (subvec (.get-data this) @i))))
                                                   (reset! signal false)
                                                   (reset! i (count (.get-data this))))
                                                 (java.util.Objects/equals e (first (get (.get-data this) @i)))
                                                 (do
                                                   (reset!
                                                     result
                                                     (vec (concat (subvec (.get-data this) 0 @i)
                                                                  [[e (+ amount (second (get (.get-data this) @i)))]]
                                                                  (subvec (.get-data this) (inc @i)))))
                                                   (reset! signal false)
                                                   (reset! i (count (.get-data this))))
                                                 )
                                           (swap! i inc)
                                           ))
                                       (if (= true @signal)
                                         (reset! result (conj (.get-data this) [e amount])))
                                       (.set-data this @result)
                                       )
                                     )
                               )))

(defn vec-remove [pos coll] (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defmulti remove class)
(defmethod remove Multiset [this]
   (fn [e] (let [i (atom 0) result (atom (.get-data this))]
                                  (do
                                    (while (< @i (count (.get-data this)))
                                      (do
                                        (if (java.util.Objects/equals e (first (get (.get-data this) @i)))
                                          (cond
                                            (= (second (get (.get-data this) @i)) 1)
                                            (reset! result (vec-remove @i (.get-data this)))
                                            :else
                                            (reset!
                                              result
                                              (assoc (.get-data this) @i [e (dec (second (get (.get-data this) @i)))]))))
                                        (swap! i inc)))
                                    (.set-data this @result)))))

(defn intersect [multisetA multisetB]
    (let [pointerA (atom 0) pointerB (atom 0) result (Multiset. [])]
                                        (do
                                          (while (and
                                              (< @pointerA (count (.get-data multisetA)))
                                              (< @pointerB (count (.get-data multisetB))))
                                            (cond
                                              (<
                                                (hash (first (get (.get-data multisetA) @pointerA)))
                                                (hash (first (get (.get-data multisetB) @pointerB))))
                                              (swap! pointerA inc)
                                              (>
                                                (hash (first (get (.get-data multisetA) @pointerA)))
                                                (hash (first (get (.get-data multisetB) @pointerB))))
                                              (swap! pointerB inc)
                                              :else
                                              (do
                                                ((add-advanced result)
                                                                       (first (get (.get-data multisetA) @pointerA))
                                                                       (Math/min
                                                                            (second (get (.get-data multisetA) @pointerA))
                                                                            (second (get (.get-data multisetB) @pointerB))))
                                                (swap! pointerA inc)
                                                (swap! pointerB inc))
                                              ))
                                          result)))

(defn union [multisetA multisetB] (let [pointerA (atom 0) pointerB (atom 0) result (Multiset. [])]
                                        (do
                                          (while (and
                                                   (< @pointerA (count (.get-data multisetA)))
                                                   (< @pointerB (count (.get-data multisetB))))
                                            (cond
                                              (<
                                                (hash (first (get (.get-data multisetA) @pointerA)))
                                                (hash (first (get (.get-data multisetB) @pointerB))))
                                              (do
                                                ((add-advanced result)
                                                                 (first (get (.get-data multisetA) @pointerA))
                                                                 (second (get (.get-data multisetA) @pointerA)))
                                                (swap! pointerA inc))
                                              (>
                                                (hash (first (get (.get-data multisetA) @pointerA)))
                                                (hash (first (get (.get-data multisetB) @pointerB))))
                                              (do
                                                ((add-advanced result)
                                                                 (first (get (.get-data multisetB) @pointerB))
                                                                 (second (get (.get-data multisetB) @pointerB)))
                                                (swap! pointerB inc))
                                              :else
                                              (do
                                                ((add-advanced result)
                                                                 (first (get (.get-data multisetA) @pointerA))
                                                                 (Math/max
                                                                          (second (get (.get-data multisetA) @pointerA))
                                                                          (second (get (.get-data multisetB) @pointerB))))
                                                (swap! pointerA inc)
                                                (swap! pointerB inc))
                                              ))
                                          (while (< @pointerA (count (.get-data multisetA)))
                                            (do
                                              ((add-advanced result)
                                                               (first (get (.get-data multisetA) @pointerA))
                                                               (second (get (.get-data multisetA) @pointerA)))
                                              (swap! pointerA inc)))
                                          (while (< @pointerB (count (.get-data multisetB)))
                                            (do
                                              ((add-advanced result)
                                                               (first (get (.get-data multisetB) @pointerB))
                                                               (second (get (.get-data multisetB) @pointerB)))
                                              (swap! pointerB inc)))
                                          result)))

(def x (Multiset. []))
(def y (Multiset. []))

((add x) 2)
((add x) 2)
((add x) 2)
((add x) 3)
((add x) "a")
((add x) "b")
((add x) "cde")
((add x) "cde")
(println "Multiset1:" (.toString x))

((add y) 2)
((add y) 2)
((add y) 3)
((add y) 4)
((add y) "a")
((add y) "c")
((add y) "cdef")
((add y) "cdef")
(println "Multiset2:" (.toString y))

(println "Union of Multiset1 and Multiset2:"(.toString (union x y)))
(println "Intersection of Multiset1 and Multiset2:" (.toString (intersect x y)))

((remove x) "cde")
(println "Multiset1 after removing cde:" (.toString x))
((remove x) "cde")
(println "Multiset1 after removing cde:" (.toString x))
((remove y) "ccc")
(println "Multiset2 after removing ccc:" (.toString y))
