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
(defn split [string] (clojure.string/split string #""))
(declare i a b c d)
;;(def i (atom 0))
;;(def a (atom []))
(defn first-stage [tokens] (while (< @i (count tokens)) (cond (or (= "0" (get tokens @i)) (= "1" (get tokens @i))) (do (swap! a conj (Constant (constants-map (get tokens @i)))) (swap! i inc)) (contains? variables (get tokens @i)) (do (swap! a conj (Variable (get tokens @i))) (swap! i inc)) :else (do (swap! a conj (get tokens @i)) (swap! i inc)))))
;;(def b (atom []))
(defn second-stage [tokens] (while (< @i (count tokens)) (cond (= "~" (get @a @i)) (do (swap! i inc) (swap! b conj (Negate (get @a @i))) (swap! i inc)) :else (do (swap! b conj (get @a @i)) (swap! i inc)))))
;;(def c (atom []))
(defn third-stage [tokens] (while (< @i (count tokens)) (cond (= "&" (get @b @i)) (do (swap! c #(assoc % (dec (count @c)) (And (get @c (dec (count @c))) (get @b (+ 1 @i))))) (swap! i #(+ 2 %))) :else (do (swap! c conj (get @b @i)) (swap! i inc)))))
;;(def d (atom (get @c 0)))
(defn fourth-stage [tokens] (while (< @i (count tokens)) (cond (= "|" (get @c @i)) (do (swap! d #(Or % (get @c (+ 1 @i)))) (swap! i #(+ 2 %))) :else (swap! i inc))))
(defn parse [tokens] (do (def i (atom 0)) (def a (atom [])) (first-stage (split tokens)) (reset! i 0) (def b (atom [])) (second-stage @a) (reset! i 0) (def c (atom [])) (third-stage @b) (reset! i 0) (def d (atom (get @c 0))) (fourth-stage @c)))
