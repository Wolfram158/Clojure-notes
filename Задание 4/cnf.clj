(defn evaluate [arg args]
  ((:evaluate arg) args))

(defn toString [arg]
  (:toString arg))

(defn Variable [arg]
  {
   :evaluate (fn [env] (env arg))
   :toString (str arg)
   :signature "variable"
   })

(defn Constant [const]
  {
   :evaluate (fn [_] const)
   :toString (str const)
   :signature "const"
   })

(defn operation [op f]
  (fn [& operands]
    {
     :evaluate (fn [args] (apply f (mapv #(evaluate % args) operands)))
     :toString (str "(" op " " (clojure.string/join " " (map toString operands)) ")")
     :signature op
     }))

(def And (operation "&" #(and %1 %2)))
(def Or (operation "|" #(or %1 %2)))
(def Negate (operation "~" not))

(def operations (set '("~" "&" "|")))
(def variables (set '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m")))
(def False (Constant false))
(def True (Constant true))
(def constants-map {"0" False "1" True})
(defn split [string] (atom (clojure.string/split string #"")))

(defn parse [tokens] (let [i (atom 0) a (atom []) z (atom []) b (atom []) c (atom [])] (do
                                                     (while (< @i (count @tokens))
                                                       (do
                                                         (cond
                                                           (or (= "0" (get @tokens @i)) (= "1" (get @tokens @i)))
                                                           (do (swap! a conj (constants-map (get @tokens @i))))
                                                           (contains? variables (get @tokens @i))
                                                           (do (swap! a conj (Variable (get @tokens @i))))
                                                           (contains? operations (get @tokens @i))
                                                           (do (swap! a conj (get @tokens @i)))
                                                           )
                                                         (if
                                                           (= "(" (get @tokens @i))
                                                           (do
                                                             ;;(def z (atom []))
                                                             (swap! i inc)
                                                             (reset! z [])
                                                             (def left (atom 1))
                                                             (def right (atom 0))
                                                             (while
                                                               (not= @right @left)
                                                               (do
                                                                 (cond
                                                                   (= "(" (get @tokens @i))
                                                                   (swap! left inc)
                                                                   (= ")" (get @tokens @i))
                                                                   (swap! right inc))
                                                                 (cond
                                                                   (or (not= @left @right) (not= (get @tokens @i) ")"))
                                                                   (swap! z conj (get @tokens @i)))
                                                                 (cond
                                                                   (not= @left @right)
                                                                   (swap! i inc))))
                                                             (swap! a conj @(parse z))
                                                             )
                                                           )
                                                         (swap! i inc)))
                                                     (do
                                                       (reset! i 0)
                                                       ;;(def b (atom []))
                                                       (while
                                                         (< @i (count @a))
                                                         (cond
                                                           (= "~" (get @a @i))
                                                           (do
                                                             (swap! i inc)
                                                             (swap! b conj (Negate (get @a @i)))
                                                             (swap! i inc))
                                                           :else
                                                           (do
                                                             (swap! b conj (get @a @i))
                                                             (swap! i inc))))
                                                       (reset! i 0)
                                                       (while
                                                         (< @i (count @b))
                                                         (cond
                                                           (= "&" (get @b @i))
                                                           (do
                                                             (swap! c #(assoc % (dec (count @c)) (And (get @c (dec (count @c))) (get @b (+ 1 @i)))))
                                                             (swap! i #(+ 2 %)))
                                                           :else
                                                           (do
                                                             (swap! c conj (get @b @i))
                                                             (swap! i inc))))
                                                       (reset! i 0)
                                                       (def d (atom (get @c 0)))
                                                       (while
                                                         (< @i (count @c))
                                                         (cond (= "|" (get @c @i))
                                                               (do
                                                                 (swap! d #(Or % (get @c (+ 1 @i))))
                                                                 (swap! i #(+ 2 %)))
                                                               :else
                                                               (swap! i inc))))
                                                     d )))
(defn analyze [string] (do 
                           (atom 
                            (map 
                             str 
                             (filter #(and (< (int %) 123) (> (int %) 96)) string)))))

(defn get-map [variables-list] 
                   (atom 
                    (into {} 
                          (map vector @variables-list (vec (for [i (range (count @variables-list))] (do 0)))))))

(defn increment [values] (let [c (atom 1) key (vec (keys @values)) i (atom 0)] 
                     (while 
                       (< @i (count key)) 
                       (do 
                         (cond 
                           (and (= (get @values (get key @i)) 1) (= @c 1)) 
                           (do (swap! values #(assoc % (get key @i) 0))) 
                           (and (= (get @values (get key @i)) 0) (= @c 1)) 
                           (do (swap! values #(assoc % (get key @i) 1)) (reset! c 0))) (swap! i inc) i))))

(defn convert [values] (into {} 
                       (map vector (keys @values) (for [[k t] @values] (cond (= t 1) true :else false)))))

(defn build-term [values] (let [res (atom [])] 
                      (for [k (keys @values)] 
                        (cond 
                          (= 1 (get @values k)) 
                          (swap! res conj (str "~" k)) 
                          :else (swap! res conj k)))))

(defn cnf [string] (let [i (atom 0) result (atom []) expr (parse (split string)) values (get-map (analyze string))]
               (do
                 (while
                     (< @i (Math/pow 2 (count @values)))
                     (do
                       (cond
                         (= false (evaluate @expr (convert values)))
                         (swap! result conj (str "(" (clojure.string/join "|" (last (build-term values))) ")")))
                       (increment values)
                       (swap! i inc)
                       )
                     )
                 (clojure.string/join "&" @result))
               ))
