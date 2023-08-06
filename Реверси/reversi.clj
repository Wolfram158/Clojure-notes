(defn BoardConfiguration [] (let [board
                                  (atom (apply vector
                                               (take 8 (repeat
                                                         (apply vector
                                                                (take 8
                                                                      (repeat ".")))))))]
                              (do
                                (swap! board #(assoc % 3 (assoc (get % 3) 3 "X")))
                                (swap! board #(assoc % 4 (assoc (get % 4) 4 "X")))
                                (swap! board #(assoc % 3 (assoc (get % 3) 4 "O")))
                                (swap! board #(assoc % 4 (assoc (get % 4) 3 "O")))
                                {:board board
                                 :printBoard (fn [] (do
                                                       (println "    1 2 3 4 5 6 7 8")
                                                       (println "    _ _ _ _ _ _ _ _")
                                                       (println "1 |" (get (mapv #(clojure.string/join #" " %) @board) 0))
                                                       (println "2 |" (get (mapv #(clojure.string/join #" " %) @board) 1))
                                                       (println "3 |" (get (mapv #(clojure.string/join #" " %) @board) 2))
                                                       (println "4 |" (get (mapv #(clojure.string/join #" " %) @board) 3))
                                                       (println "5 |" (get (mapv #(clojure.string/join #" " %) @board) 4))
                                                       (println "6 |" (get (mapv #(clojure.string/join #" " %) @board) 5))
                                                       (println "7 |" (get (mapv #(clojure.string/join #" " %) @board) 6))
                                                       (println "8 |" (get (mapv #(clojure.string/join #" " %) @board) 7))))})))

(def signMap {"X" "O" "O" "X"})

(defn checkCell [i j sign board] (let [counter (atom 0) i1 (atom 0) j1 (atom 0)]
                                   (do
                                     (reset! i1 (dec i))
                                     (reset! j1 j)
                                     (while (and (pos? @i1) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                       (do
                                         (swap! counter inc)
                                         (swap! i1 dec)))
                                     (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                           true
                                           :else
                                            (do
                                             (reset! counter 0)
                                             (reset! i1 (inc i))
                                             (reset! j1 j)
                                             (while (and (< @i1 7) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                               (do
                                                 (swap! counter inc)
                                                 (swap! i1 inc)))
                                             (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                   true
                                                   :else
                                                   (do
                                                     (reset! counter 0)
                                                     (reset! i1 i)
                                                     (reset! j1 (inc j))
                                                     (while (and (< @j1 7) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                       (do
                                                         (swap! counter inc)
                                                         (swap! j1 inc)))
                                                     (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                           true
                                                           :else
                                                           (do
                                                             (reset! counter 0)
                                                             (reset! i1 i)
                                                             (reset! j1 (dec j))
                                                             (while (and (pos? @j1) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                               (do
                                                                 (swap! counter inc)
                                                                 (swap! j1 dec)))
                                                             (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                                   true
                                                                   :else
                                                                   (do
                                                                     (reset! counter 0)
                                                                     (reset! i1 (inc i))
                                                                     (reset! j1 (inc j))
                                                                     (while (and (< @i1 7) (< @j1 7) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                                       (do
                                                                         (swap! counter inc)
                                                                         (swap! i1 inc)
                                                                         (swap! j1 inc)))
                                                                     (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                                           true
                                                                           :else
                                                                           (do
                                                                             (reset! counter 0)
                                                                             (reset! i1 (dec i))
                                                                             (reset! j1 (dec j))
                                                                             (while (and (pos? @i1) (pos? @j1) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                                               (do
                                                                                 (swap! counter inc)
                                                                                 (swap! i1 dec)
                                                                                 (swap! j1 dec)))
                                                                             (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                                                   true
                                                                                   :else
                                                                                   (do
                                                                                     (reset! counter 0)
                                                                                     (reset! i1 (inc i))
                                                                                     (reset! j1 (dec j))
                                                                                     (while (and (pos? @j1) (< @i1 7) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                                                       (do
                                                                                         (swap! counter inc)
                                                                                         (swap! i1 inc)
                                                                                         (swap! j1 dec)))
                                                                                     (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                                                           true
                                                                                           :else
                                                                                           (do
                                                                                             (reset! counter 0)
                                                                                             (reset! i1 (dec i))
                                                                                             (reset! j1 (inc j))
                                                                                             (while (and (pos? @i1) (< @j1 7) (= (get signMap sign) (get (get @(:board board) @i1) @j1)))
                                                                                               (do
                                                                                                 (swap! counter inc)
                                                                                                 (swap! i1 dec)
                                                                                                 (swap! j1 inc)))
                                                                                             (cond (and (pos? @counter) (= "." (get (get @(:board board) @i1) @j1)))
                                                                                                   true
                                                                                                   :else
                                                                                                   false))))))))))))))))))

(defn canMakeMove [board sign] (let [i (atom 0) j (atom 0) result (atom false)]
                                 (do
                                   (while (< @i 8)
                                     (do
                                       (while (< @j 8)
                                         (do
                                           (if
                                             (and (= sign (get (get @(:board board) @i) @j)) (= true (checkCell @i @j sign board)))
                                             (reset! result true))
                                           (swap! j inc)))
                                       (swap! i inc)
                                       (reset! j 0)))
                                   @result)))

(defn isValidMove [x1 y1 x2 y2 board sign] (let [i (atom 0) j (atom 0) result (atom true)]
                                             (do
                                               (cond (= x1 x2)
                                                     (cond (< y1 y2)
                                                           (do
                                                             (reset! i (inc y1))
                                                             (while (< @i y2)
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) x1) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i inc))))
                                                           :else
                                                           (do
                                                             (reset! i (dec y1))
                                                             (while (> @i y2)
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) x1) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i dec)))))
                                                     (= y1 y2)
                                                     (cond (< x1 x2)
                                                           (do
                                                             (reset! i (inc x1))
                                                             (while (< @i x2)
                                                               (do
                                                                 (if (not= (get (get @(:board board) y1) @i) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i inc))))
                                                           :else
                                                           (do
                                                             (reset! i (dec x1))
                                                             (while (> @i x2)
                                                               (do
                                                                 (if (not= (get (get @(:board board) y1) @i) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i dec)))))
                                                     :else
                                                     (cond (and (< x1 x2) (< y1 y2))
                                                           (do
                                                             (reset! i (inc y1))
                                                             (reset! j (inc x1))
                                                             (while (and (< @i y2) (< @j x2))
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) @j) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i inc)
                                                                 (swap! j inc))))
                                                           (and (> x1 x2) (< y1 y2))
                                                           (do
                                                             (reset! i (inc y1))
                                                             (reset! j (dec x1))
                                                             (while (and (< @i y2) (> @j x2))
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) @j) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i inc)
                                                                 (swap! j dec))))
                                                           (and (< x1 x2) (> y1 y2))
                                                           (do
                                                             (reset! i (dec y1))
                                                             (reset! j (inc x1))
                                                             (while (and (> @i y2) (< @j x2))
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) @j) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i dec)
                                                                 (swap! j inc))))
                                                           (and (> x1 x2) (> y1 y2))
                                                           (do
                                                             (reset! i (dec y1))
                                                             (reset! j (dec x1))
                                                             (while (and (> @i y2) (> @j x2))
                                                               (do
                                                                 (if (not= (get (get @(:board board) @i) @j) (get signMap sign))
                                                                   (reset! result false))
                                                                 (swap! i dec)
                                                                 (swap! j dec))))))
                                               @result)))

(defn makeMoveHumanImpl [board sign] (let [signal (atom true) input (atom "") x1 (atom 0) y1 (atom 0) x2 (atom 0) y2 (atom 0) i (atom 0) j (atom 0)]
                                       (while (= true @signal)
                                         (do
                                           (reset! input (read-line))
                                           (swap! input #(clojure.string/split % #" "))
                                           (reset! x1 (dec (Integer/parseInt (get @input 0))))
                                           (reset! y1 (dec (Integer/parseInt (get @input 1))))
                                           (reset! x2 (dec (Integer/parseInt (get @input 2))))
                                           (reset! y2 (dec (Integer/parseInt (get @input 3))))
                                           (if (and (or (and (not= @y1 @y2) (= (Math/abs (- @y1 @y2)) (Math/abs (- @x1 @x2))))
                                                        (and (= @y1 @y2) (< 1 (Math/abs (- @x1 @x2))))
                                                        (and (= @x1 @x2) (< 1 (Math/abs (- @y1 @y2)))))
                                                    (<= @x1 7)
                                                    (>= @x1 0)
                                                    (<= @y1 7)
                                                    (>= @y1 0)
                                                    (<= @x2 7)
                                                    (>= @x2 0)
                                                    (<= @y2 7)
                                                    (>= @y2 0)
                                                    (isValidMove @x1 @y1 @x2 @y2 board sign)
                                                    (= (get (get @(:board board) @y1) @x1) sign)
                                                    (= (get (get @(:board board) @y2) @x2) "."))
                                             (do
                                               (reset! signal false)
                                               (cond (= @x1 @x2)
                                                     (cond (< @y1 @y2)
                                                           (do
                                                             (reset! i (inc @y1))
                                                             (while (<= @i @y2)
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                                  @i
                                                                                                  (assoc
                                                                                                    (get % @i)
                                                                                                    @x1
                                                                                                    sign)))
                                                                 (swap! i inc))))
                                                           :else
                                                           (do
                                                             (reset! i (dec @y1))
                                                             (while (>= @i @y2)
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                                  @i
                                                                                                  (assoc
                                                                                                    (get % @i)
                                                                                                    @x1
                                                                                                    sign)))
                                                                 (swap! i dec)))))
                                                     (= @y1 @y2)
                                                     (cond (< @x1 @x2)
                                                           (do
                                                             (reset! i (inc @x1))
                                                             (while (<= @i @x2)
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                                  @y1
                                                                                                  (assoc
                                                                                                    (get % @y1)
                                                                                                    @i
                                                                                                    sign)))
                                                                 (swap! i inc))))
                                                           :else
                                                           (do
                                                             (reset! i (dec @x1))
                                                             (while (>= @i @x2)
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                                  @y1
                                                                                                  (assoc
                                                                                                    (get % @y1)
                                                                                                    @i
                                                                                                    sign)))
                                                                 (swap! i dec)))))
                                                     :else
                                                     (cond (and (< @x1 @x2) (< @y1 @y2))
                                                           (do
                                                             (reset! i (inc @y1))
                                                             (reset! j (inc @x1))
                                                             (while (and (<= @i @y2) (<= @j @x2))
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                                  @i
                                                                                                  (assoc
                                                                                                    (get % @i)
                                                                                                    @j
                                                                                                    sign)))
                                                                 (swap! i inc)
                                                                 (swap! j inc))))
                                                           (and (> @x1 @x2) (< @y1 @y2))
                                                           (do
                                                             (reset! i (inc @y1))
                                                             (reset! j (dec @x1))
                                                             (while (and (<= @i @y2) (>= @j @x2))
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                          @i
                                                                                          (assoc
                                                                                            (get % @i)
                                                                                            @j
                                                                                            sign)))
                                                                 (swap! i inc)
                                                                 (swap! j dec))))
                                                           (and (< @x1 @x2) (> @y1 @y2))
                                                           (do
                                                             (reset! i (dec @y1))
                                                             (reset! j (inc @x1))
                                                             (while (and (>= @i @y2) (<= @j @x2))
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                          @i
                                                                                          (assoc
                                                                                            (get % @i)
                                                                                            @j
                                                                                            sign)))
                                                                 (swap! i dec)
                                                                 (swap! j inc))))
                                                           (and (> @x1 @x2) (> @y1 @y2))
                                                           (do
                                                             (reset! i (dec @y1))
                                                             (reset! j (dec @x1))
                                                             (while (and (>= @i @y2) (>= @j @x2))
                                                               (do
                                                                 (swap! (:board board) #(assoc %
                                                                                          @i
                                                                                          (assoc
                                                                                            (get % @i)
                                                                                            @j
                                                                                            sign)))
                                                                 (swap! i dec)
                                                                 (swap! j dec)))))))
                                             (println "Incorrect move. Try again."))))))

(defn Player [strategy sign name] {:makeMove (fn [board] (strategy board sign))
                                   :sign sign
                                   :name name})

(def HumanPlayer (partial Player makeMoveHumanImpl))

(defn GameServer [board players] (let [whoseTurn (atom 0) endFactor (atom 0) countX (atom 0) countO (atom 0) i (atom 0) j (atom 0)]
                                         (do
                                           ((:printBoard board))
                                           (while (not= @endFactor 2)
                                             (cond
                                               (= true (canMakeMove board (get (get players @whoseTurn) :sign)))
                                               (do
                                                 (println "Player" (get (get players @whoseTurn) :name) "is moving.")
                                                 ((:makeMove (get players @whoseTurn)) board)
                                                 ((:printBoard board))
                                                 (reset! endFactor 0)
                                                 (reset! whoseTurn (rem (inc @whoseTurn) (count players)))
                                                 )
                                               :else
                                               (swap! endFactor inc)
                                               (println "Player" (get (get players @whoseTurn) :name) "can't make move.")
                                               (reset! whoseTurn (rem (inc @whoseTurn) (count players)))))
                                           (while (< @i 8)
                                             (while (< @j 8)
                                               (cond
                                                 (= "X" (get (get @(:board board) @i) @j))
                                                 (swap! countX inc)
                                                 (= "O" (get (get @(:board board) @i) @j))
                                                 (swap! countO inc))))
                                           (cond
                                             (= @countX @countO)
                                             (println "No one has won.")
                                             (> @countX @countO)
                                             (do
                                               (reset! i 0)
                                               (while (< @i (count players))
                                                 (do
                                                   (cond
                                                     (= "X" (get (get players @i) :sign))
                                                     (println "Player" (get (get players @i) :name) "has won"))
                                                   (swap! i inc))))
                                             :else
                                             (do
                                               (reset! i 0)
                                               (while (< @i (count players))
                                                 (do
                                                   (cond
                                                     (= "O" (get (get players @i) :sign))
                                                     (println "Player" (get (get players @i) :name) "has won"))
                                                   (swap! i inc))))))))

(def board (BoardConfiguration))
(def player1 (HumanPlayer "X" "Peter"))
(def player2 (HumanPlayer "O" "Paul"))
(GameServer board [player1 player2])

;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . . . . . .
;; 4 | . . . X O . . .
;; 5 | . . . O X . . .
;; 6 | . . . . . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Peter is moving.
;; 4 4 4 6
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . . . . . .
;; 4 | . . . X O . . .
;; 5 | . . . X X . . .
;; 6 | . . . X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Paul is moving.
;; 5 4 3 6
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . . . . . .
;; 4 | . . . X O . . .
;; 5 | . . . O X . . .
;; 6 | . . O X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Peter is moving.
;; 5 5 6 5
;; Incorrect move. Try again.
;; 5 5 5 3
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . . X . . .
;; 4 | . . . X X . . .
;; 5 | . . . O X . . .
;; 6 | . . O X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Paul is moving.
;; 4 5 4 4
;; Incorrect move. Try again.
;; 4 5 4 3
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . O X . . .
;; 4 | . . . O X . . .
;; 5 | . . . O X . . .
;; 6 | . . O X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Peter is moving.
;; 5 4 3 4
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . O X . . .
;; 4 | . . X X X . . .
;; 5 | . . . O X . . .
;; 6 | . . O X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Paul is moving.
;; 4 5 6 3
;;     1 2 3 4 5 6 7 8
;;     _ _ _ _ _ _ _ _
;; 1 | . . . . . . . .
;; 2 | . . . . . . . .
;; 3 | . . . O X O . .
;; 4 | . . X X O . . .
;; 5 | . . . O X . . .
;; 6 | . . O X . . . .
;; 7 | . . . . . . . .
;; 8 | . . . . . . . .
;; Player Peter is moving.
