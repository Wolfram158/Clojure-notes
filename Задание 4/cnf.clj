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
(def constants-map {"0" false "1" true})
(defn split [string] (let [tokens (atom (clojure.string/split string #""))] tokens))

(defn parse [tokens] (let [i (atom 0) a (atom []) z (atom []) b (atom []) c (atom [])] (do
                                                     (while (< @i (count @tokens))
                                                       (do
                                                         (cond
                                                           (or (= "0" (get @tokens @i)) (= "1" (get @tokens @i)))
                                                           (do (swap! a conj (Constant (constants-map (get @tokens @i)))))
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
                                                             ;;(println @(parse z))
                                                             )
                                                           )
                                                         (swap! i inc)))
                                                     ;;(println "Number " @i @a)
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
                                                       ;;(def c (atom []))
                                                       ;;(println "List b:" b)
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
                                                       ;;(println "List c: " @c)
                                                       (while
                                                         (< @i (count @c))
                                                         (cond (= "|" (get @c @i))
                                                               (do
                                                                 (swap! d #(Or % (get @c (+ 1 @i))))
                                                                 (swap! i #(+ 2 %)))
                                                               :else
                                                               (swap! i inc))))
                                                     d )))
