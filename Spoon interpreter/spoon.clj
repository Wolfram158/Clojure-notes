(defn subs [s i j] (let [counter (atom i) result (atom "")] (do
                                                              (while (and (< @counter j) (< i (count s)))
                                                                (do
                                                                  (reset! result (str @result (get s @counter)))
                                                                  (swap! counter inc)))
                                                              @result)))

(defn abstract-test [word] (fn [s i] (= (subs s i (+ i (count word))) word)))

(def test-inc (abstract-test "1"))
(def test-dec (abstract-test "000"))
(def test-next (abstract-test "010"))
(def test-prev (abstract-test "011"))
(def test-cycle-begin (abstract-test "00100"))
(def test-cycle-end (abstract-test "0011"))
(def test-input (abstract-test "0010110"))
(def test-print (abstract-test "001010"))

(defn spoonToBF [s] (let [i (atom 0) result (atom "")]
                      (do
                        (while (< @i (count s))
                          (cond
                            (test-inc s @i)
                            (do
                              (swap! result #(str % "+"))
                              (swap! i inc))
                            (test-dec s @i)
                            (do
                              (swap! result #(str % "-"))
                              (swap! i #(+ % 3)))
                            (test-next s @i)
                            (do
                              (swap! result #(str % ">"))
                              (swap! i #(+ % 3)))
                            (test-prev s @i)
                            (do
                              (swap! result #(str % "<"))
                              (swap! i #(+ % 3)))
                            (test-cycle-begin s @i)
                            (do
                              (swap! result #(str % "["))
                              (swap! i #(+ % 5)))
                            (test-cycle-end s @i)
                            (do
                              (swap! result #(str % "]"))
                              (swap! i #(+ % 4)))
                            (test-input s @i)
                            (do
                              (swap! result #(str % ","))
                              (swap! i #(+ % 7)))
                            (test-print s @i)
                            (do
                              (swap! result #(str % "."))
                              (swap! i #(+ % 6)))
                            :else
                            (do
                              (println "Unsupported command begins at:" @i)
                              (reset! i (count s))))
                          )
                        @result)))

(defn interpret [input] (let [arr (atom (apply vector (take 30000 (repeat 0)))) i (atom 0)
                              j (atom 0) s (spoonToBF input) result (atom "") stack (atom [])
                              left (atom 1) right (atom 0)]
                          (do
                            (while (< @j (count s))
                              (do
                                (cond
                                  (= (get s @j) \>)
                                  (swap! i inc)
                                  (= (get s @j) \<)
                                  (swap! i dec)
                                  (= (get s @j) \+)
                                  (swap! arr #(assoc % @i (inc (get @arr @i))))
                                  (= (get s @j) \-)
                                  (swap! arr #(assoc % @i (dec (get @arr @i))))
                                  (= (get s @j) \.)
                                  (reset! result (str @result (char (get @arr @i))))
                                  (= (get s @j) \,)
                                  (swap! arr #(assoc % @i (read-line)))
                                  (= (get s @j) \[)
                                  (cond
                                    (= 0 (get @arr @i))
                                    (do
                                      (reset! left 1)
                                      (reset! right 0)
                                      (while (not= @left @right)
                                        (do
                                          (if
                                            (= (get s @j) \[)
                                            (swap! left inc))
                                          (if
                                            (= (get s @j) \])
                                            (swap! right inc))
                                          (swap! j inc))))
                                    :else
                                    (swap! stack #(conj % @j)))
                                  (= (get s @j) \])
                                  (cond
                                    (= 0 (get @arr @i))
                                    (swap! stack #(pop %))
                                    :else
                                    (reset! j (last @stack))))
                                (swap! j inc)))
                            @result)))

(do
  (println (interpret "1111111111001000101111110101111111111011011000001101011111110010100101111111100101011100101000000000000000000101011111111111001010000000000001010000000000000000000000000000000000000000001010"))
  (println (interpret "11111111110010001011111110101111111111010111010101101101101100000110101100101001010010101111111001010001010111001010010110010100110111111111111111110010100100010101110010100000000000000000000010100000000000000000000000000010100101001010010001010")))
