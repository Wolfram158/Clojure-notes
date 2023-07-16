(defn evaluate [arg args]
  ((:evaluate arg) args))

(defn toString [arg]
  (:toString arg))

(defn Variable [arg]
  {
   :evaluate (fn [env] (env arg))
   :toString (str arg)
   :toMiniString (str arg)
   :signature "variable"
   :priority 0
   })

(defn Constant [const]
  {
   :evaluate (fn [_] const)
   :toString (str const)
   :toMiniString (cond (= const true) (str "1") :else (str "0"))
   :value const
   :signature "const"
   :priority 0
   })

(def False (Constant false))
(def True (Constant true))
(def constants-map {"0" False "1" True})

(defn operation [op priority f g]
  (fn [& operands]
    {
     :evaluate (fn [args] (apply f (mapv #(evaluate % args) operands)))
     :toString (str "(" op " " (clojure.string/join " " (map toString operands)) ")")
     :toMiniString (apply g operands)
     :signature op
     :priority priority
     :l (first operands)
     :r (second operands)
     }))

(def And (operation "&" 2 #(and %1 %2)
                    #(cond (and (= (get %1 :priority) 1) (= (get %2 :priority) 1))
                           (str "(" (get %1 :toMiniString) ")&(" (get %2 :toMiniString) ")")
                           (= (get %1 :priority) 1)
                           (str "(" (get %1 :toMiniString) ")&" (get %2 :toMiniString))
                           (= (get %2 :priority) 1)
                           (str (get %1 :toMiniString) "&(" (get %2 :toMiniString) ")")
                           :else
                           (str (get %1 :toMiniString) "&" (get %2 :toMiniString))
                           )))
(def Or (operation "|" 1 #(or %1 %2)
                   #(str (get %1 :toMiniString) "|" (get %2 :toMiniString))))
(def Negate (operation "~" 3 not
                       #(cond (> (get %1 :priority) 0)
                              (cond (not= (get %1 :priority) 3)
                                    (str "~(" (get (get %1 :l) :toMiniString) (get %1 :signature)
                                         (get (get %1 :r) :toMiniString) ")")
                                    :else
                                    (str "~(" (get %1 :toMiniString) ")"))
                              :else
                              (str "~" (get %1 :toMiniString)))))

(defn push [expr] (cond (= (get expr :signature) "&")
                        (And (push (get expr :l)) (push (get expr :r)))
                        (= (get expr :signature) "|")
                        (Or (push (get expr :l)) (push (get expr :r)))
                        (= (get expr :signature) "~")
                        (cond (= (get (get expr :l) :signature) "&")
                              (push (Or (push (Negate (get (get expr :l) :l))) (push (Negate (get (get expr :l) :r)))))
                              (= (get (get expr :l) :signature) "|")
                              (push (And (push (Negate (get (get expr :l) :l))) (push (Negate (get (get expr :l) :r)))))
                              (= (get (get expr :l) :signature) "const")
                              (cond (= (get (get expr :l) :value) false)
                                    (get constants-map "1")
                                    :else
                                    (get constants-map "0"))
                              (= (get (get expr :l) :signature) "variable")
                              (Negate (push (get expr :l)))
                              :else
                              (push (get (get expr :l) :l)))
                        (= (get expr :signature) "variable")
                        (Variable (get expr :toString))
                        :else
                        (Constant (get expr :value))))

(defn simplify [expr] (cond (= (get expr :signature) "&")
                            (cond (= (get (get expr :l) :signature) "const")
                                  (cond (= (get (get expr :l) :value) false)
                                        False
                                        :else
                                        (simplify (get expr :r)))
                                  (= (get (get expr :r) :signature) "const")
                                  (cond (= (get (get expr :r) :value) false)
                                        False
                                        :else
                                        (simplify (get expr :l)))
                                  (and (not= (get (simplify (get expr :l)) :signature) "const") (not= (get (simplify (get expr :r)) :signature) "const"))
                                  (And (simplify (get expr :l)) (simplify (get expr :r)))
                                  :else
                                  (simplify (And (simplify (get expr :l)) (simplify (get expr :r))))
                                  )
                            (= (get expr :signature) "|")
                            (cond (= (get (get expr :l) :signature) "const")
                                  (cond (= (get (get expr :l) :value) true)
                                        True
                                        :else
                                        (simplify (get expr :r)))
                                  (= (get (get expr :r) :signature) "const")
                                  (cond (= (get (get expr :r) :value) true)
                                        True
                                        :else
                                        (simplify (get expr :l)))
                                  (and (not= (get (simplify (get expr :l)) :signature) "const") (not= (get (simplify (get expr :r)) :signature) "const"))
                                  (Or (simplify (get expr :l)) (simplify (get expr :r)))
                                  :else
                                  (simplify (Or (simplify (get expr :l)) (simplify (get expr :r))))
                                  )
                            (= (get expr :signature) "~")
                            (cond (= (get (get expr :l) :signature) "const")
                                  (cond (= (get (get expr :l) :value) false)
                                        True
                                        :else
                                        False)
                                  (not= (get (simplify (get expr :l)) :signature) "const")
                                  (Negate (simplify (get expr :l)))
                                  :else
                                  (simplify (Negate (simplify (get expr :l))))
                                  )
                            :else
                            expr
                            ))

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
(defn push-string [string] (get (push (parse-string string)) :toMiniString))
(defn simplify-string [string] (get (simplify (parse-string string)) :toMiniString))
(defn push-and-simplify-string [string] (get (simplify (push (parse-string string))) :toMiniString))

(do (println "Test 1: " (push-string "~(a|b)"))
    (println "Test 2: " (push-string "~(a&b)"))
    (println "Test 3: " (push-string "~(a&b|c|(d|~(f&~g)&y|e))"))
    (println "Test 4: " (push-string "a&b|~(c&d|~f|~(h|~x)|u&~v|w)&~p|t"))
    (println "Test 5: " (push-string "a&b|(c|(r|a&~(f|~g&b&n&u|~t|y)))|~a"))
    (println "Test 6: " (push-string "~a&~(b|~(c|~h&(~f|~(k|~(b&~c)))))"))
    (println "Test 7: " (push-string "~(~(~(~a|b|c&d)&e|r)|~t&f|o)"))
    (println "Test 8: " (push-string "~0|b&(~a|1&~(0|b&c|i&~0&1))|~1"))
    (println "Test 9: " (push-and-simplify-string "~0|b&(~a|1&~(0|b&c|i&~0&1))|~1"))
    (println "Test 10: " (simplify-string "~0|b&(~a|1&~(0|b&c|i&~0&1))|~1"))
    (println "Test 11: " (push-string "~(~a|~(b&c)&~i)"))
    (println "Test 12: " (push-string "~1&a|~(b|~(a&b|~(u|v&~y)|t&~0|(~(a&~(b|c|0)&1))))"))
    (println "Test 13: " (push-and-simplify-string "~1&a|~(b|~(a&b|~(u|v&~y)|t&~1|(~(a&~(b|c|0)&1))))"))
    (println "Test 14: " (simplify-string "~1&a|~(b|~(a&b|~(u|v&~y)|t&~1|(~(a&~(b|c|0)&1))))"))
    (println "Test 15: " (push-and-simplify-string "0|~(b|~c&(a|b|y&~0&i|~(u|~i&(1|0|~t|~(f|g)))|g|~(v|1)))")))
