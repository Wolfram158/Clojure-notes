(defn Text [string] {:toHTML string})
(defn Node [beginTag endTag] (fn [inside] {:toHTML (str beginTag inside endTag)}))
(def Strong (Node "<strong>" "</strong>"))
(def Emphasis (Node "<em>" "</em>"))
(def Strikeout (Node "<s>" "</s>"))
(def Underline (Node "<u>" "</u>"))
(def Code (Node "<pre>" "</pre>"))

(def tags {"[b]"  "[/b]", "[i]"  "[/i]", "[u]"  "[/u]", "[s]"  "[/s]", "[code]"  "[/code]"})
(def tagsList ["[b]", "[i]", "[u]", "[s]", "[code]"])

(defn subs [s i j] (let [counter (atom i) result (atom "")] (do
                                                              (while (and (< @counter j) (< i (count s)))
                                                                (do
                                                                  (reset! result (str @result (get s @counter)))
                                                                  (swap! counter inc)))
                                                              @result)))

(defn testTag [s i tag] (= (subs s i (+ i (count tag))) tag))

(defn parse [s] (let [i (atom 0) data (atom []) curS (atom "") signal (atom true)
                      flag (atom true) j (atom 0) k (atom 0) left (atom 1) right (atom 0)]
                  (do
                    (while (< @i (count s))
                        (do
                          (reset! signal true)
                          (doseq [tag tagsList]
                            (cond (= true (testTag s @i tag))
                                  (do
                                    (reset! signal false)
                                    (reset! j (+ @i (count tag)))
                                    (reset! k @j)
                                    (reset! left 1)
                                    (reset! right 0)
                                    (reset! flag true)
                                    (while (and (< @k (count s)) (= true @flag))
                                      (do
                                        (cond (testTag s @k (get tags tag))
                                              (swap! right inc))
                                        (cond (testTag s @k tag)
                                              (swap! left inc))
                                        (cond (= @left @right)
                                              (reset! flag false))
                                        (swap! k inc)))
                                    (cond (= false @flag)
                                          (do
                                            (swap! data #(conj % (Text @curS)))
                                            (reset! curS "")
                                            (cond
                                              (= tag "[b]")
                                              (swap! data #(conj % (Strong (parse (subs s @j (dec @k))))))
                                              (= tag "[i]")
                                              (swap! data #(conj % (Emphasis (parse (subs s @j (dec @k))))))
                                              (= tag "[u]")
                                              (swap! data #(conj % (Underline (parse (subs s @j (dec @k))))))
                                              (= tag "[s]")
                                              (swap! data #(conj % (Strikeout (parse (subs s @j (dec @k))))))
                                              (= tag "[code]")
                                              (swap! data #(conj % (Code (parse (subs s @j (dec @k)))))))
                                            (reset! i (+ @k (count tag))))
                                          :else
                                          (do
                                            (reset! curS (str @curS (get s @i)))
                                            (swap! i inc))))))
                          (if (= true @signal)
                            (do
                              (reset! curS (str @curS (get s @i)))
                              (swap! i inc)))))
                    (if (not= @curS "")
                      (swap! data #(conj % (Text @curS))))
                    (clojure.string/join "" (mapv :toHTML @data)))))

(do (println (parse "[i]Iff [s] but not [/s] why not [/i] testing [code][/code"))
    (println )
    (println (parse "[s][i][s][i][b]F is a [s][/s]function iff [i]c[/i][/b][/i][/s] but people say that [i]it is not[/i] however [s][b][i]YES[/i][/b][/s][/i][/s]"))
    (println)
    (println (parse "The [i]text[/i] is [b][i]Operation [s]not[b][/s] binary[/i][/b] [s]because[/s] [b]"))
    (println )
    (println (parse "Using namespace [i][/i] instead of [code]std[/code] is better than [b][b][/b]"))
    (println )
    (println (parse "[code][i]We use tag [b][s] when want [/s] to get strong [/i] and [/code]")))
