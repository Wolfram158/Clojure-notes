(definterface IExpression
  (^Boolean evaluate [vars]))

(deftype Const [value signature]
  IExpression
  (evaluate [_ _] value)
  Object
  (toString [_]
     (if (= value true) "1" "0")))

(defn Constant [value]
  (Const. value "const"))

(deftype VariableT [name signature]
  IExpression
  (evaluate [_ variables-map] (get variables-map name))
  Object
  (toString [_] name))

(defn Variable [name]
  (VariableT. name "variable"))

(deftype Expression [f sign args signature]
  IExpression
  (evaluate [_ variables-map] (apply f (mapv #(.evaluate % variables-map) args)))
  Object
  (toString [_] (str "(" sign " " (clojure.string/join " " (mapv #(.toString %) args)) ")")))

(defn And [& args] (Expression. #(and %1 %2) "&" args "and"))
(defn Or [& args] (Expression. #(or %1 %2) "|" args "or"))
(defn Negate [arg] (Expression. not "~" [arg] "|"))

(def False (Constant false))
(def True (Constant true))
(def constants-map {"0" False "1" True})

(defmulti push class)

(defmethod push Expression [expr]
  (cond (= (.sign expr) "&")
        (And (push (first (.args expr))) (push (second (.args expr))))
        (= (.sign expr) "|")
        (Or (push (first (.args expr))) (push (second (.args expr))))
        (= (.sign expr) "~")
        (cond (= (.signature (first (.args expr))) "and")
              (push (Or (push (Negate (first (.args (first (.args expr)))))) (push (Negate (second (.args (first (.args expr))))))))
              (= (.signature (first (.args expr))) "or")
              (push (And (push (Negate (first (.args (first (.args expr)))))) (push (Negate (second (.args (first (.args expr))))))))
              (= (.signature (first (.args expr))) "const")
              (cond (= (.value (first (.args expr))) false)
                    (get constants-map "1")
                    :else
                    (get constants-map "0"))
                    (= (.signature (first (.args expr))) "variable")
                    (Negate (push (first (.args expr))))
       :else
       (push (first (.args (first (.args expr))))))))

(defmethod push Const [expr] expr)

(defmethod push VariableT [expr] expr)

(def operations (set '("~" "&" "|")))
(def variables (set '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m")))
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

(defn parse-string [string] @(parse (split string)))
(defn push-string [string] (.toString (push (parse-string string))))
