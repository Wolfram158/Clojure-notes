(defn isRow [board row num] (let [i (atom 0) result (atom true)]
                              (do
                                (while (< @i 9)
                                  (do
                                    (if
                                      (= (get (get board row) @i) num)
                                      (reset! result false))
                                    (swap! i inc)))
                                @result)))

(defn isColumn [board col num] (let [i (atom 0) result (atom true)]
                                 (do
                                   (while (< @i 9)
                                     (do
                                       (if
                                         (= (get (get board @i) col) num)
                                         (reset! result false))
                                       (swap! i inc)))
                                   @result)))

(defn isSquare [board startRow startColumn num] (let [i (atom 0) j (atom 0) result (atom true)]
                                                  (do
                                                    (while (< @i 3)
                                                      (do
                                                        (while (< @j 3)
                                                          (do
                                                            (if (= (get (get board (+ @i startRow)) (+ @j startColumn)) num)
                                                              (reset! result false))
                                                            (swap! j inc)))
                                                        (reset! j 0)
                                                        (swap! i inc)))
                                                    @result)))

(defn printBoard [board] (let [i (atom 0) j (atom 0)]
                           (while (< @i 9)
                             (do
                               (while (< @j 9)
                                 (do
                                   (if
                                     (or (= @j 2) (= @j 5))
                                     (print (get (get board @i) @j) "| ")
                                     (print (get (get board @i) @j) ""))
                                   (swap! j inc)))
                               (println )
                               (if
                                 (or (= @i 2) (= @i 5))
                                 (println "_ _ _   _ _ _   _ _ _"))
                               (reset! j 0)
                               (swap! i inc)))))

(defn solve [board] (let [resetable (atom board) x (atom 0) y (atom 0)
                          signal (atom false) startRow (atom 0) startColumn (atom 0)
                          num (atom 1) result (atom false) i (atom 0) j (atom 0)]
                      (do
                        (while (< @i 9)
                          (do
                            (while (< @j 9)
                              (do
                                (if
                                  (= 0 (get (get board @i) @j))
                                  (do
                                    (reset! y @i)
                                    (reset! x @j)
                                    (reset! signal true)
                                    (reset! j 9)
                                    (reset! i 9)))
                                (swap! j inc)))
                            (reset! j 0)
                            (swap! i inc)))
                        (if
                          (= @signal false)
                          (do
                            (printBoard board)
                            (reset! result true))
                          (do
                            (reset! startRow (- @y (rem @y 3)))
                            (reset! startColumn (- @x (rem @x 3)))
                            (while (< @num 10)
                              (do
                                (if
                                  (and (isRow board @y @num)
                                       (isSquare board @startRow @startColumn @num)
                                       (isColumn board @x @num))
                                  (do
                                    (reset! resetable (assoc board @y (assoc (get board @y) @x @num)))
                                    (if
                                      (= true (solve @resetable))
                                      (do
                                        (reset! result true)
                                        (reset! num 10))
                                      (reset! resetable (assoc board @y (assoc (get board @y) @x 0))))))
                                (swap! num inc)))))
                        @result)))

(def x [[0 0 0 7 0 0 4 0 0] [0 8 0 5 0 0 9 0 0] [0 7 5 3 0 0 0 1 0] [0 1 0 0 2 0 6 0 0] [7 0 4 0 0 0 0 0 0] 
[6 0 0 0 8 0 0 4 0] [0 0 0 0 0 0 0 0 0] [0 6 0 8 0 0 0 0 3] [0 5 0 0 9 0 1 0 0]])
(printBoard x)
(solve x)
