(defn Board [x y] (let [set (atom #{})
                        board (atom (apply vector
                                           (take (inc (* 2 y)) (repeat
                                                                 (apply vector
                                                                        (take (inc (* 2 x))
                                                                              (repeat nil)))))))]
                       {:x x :y y :used-moves set
                        :configuration (do
                                         (loop [i 0]
                                               (when (< i (inc (* 2 y)))
                                                     (do
                                                       (if
                                                         (= (rem i 2) 1)
                                                         (swap! board #(assoc %
                                                                              i
                                                                              (apply vector
                                                                                     (take (inc (* 2 x))
                                                                                           (repeat " ")))))
                                                         (loop [j 0]
                                                               (when (< j (inc (* 2 x)))
                                                                     (do
                                                                       (if (= (rem j 2) 1)
                                                                         (swap! board #(assoc % i (assoc (get % i) j " ")))
                                                                         (swap! board #(assoc % i (assoc (get % i) j "."))))
                                                                       (recur (inc j))))))
                                                       (recur (inc i)))))
                                         board)
                        :print-board #(doseq [row @board]
                                             (println (clojure.string/join "  " row)))}))

(defn Point [x y] {:x x :y y})
(defn Stick [point1 point2] (cond (= (:x point1) (:x point2))
                                  (cond (< (:y point1) (:y point2))
                                        {:point1 point1 :point2 point2}
                                        :else
                                        {:point1 point2 :point2 point1})
                                  :else
                                  (cond (< (:x point1) (:x point2))
                                        {:point1 point1 :point2 point2}
                                        :else
                                        {:point1 point2 :point2 point1})))

(defn HumanPlayer [name sign] (let [score (atom 0)]
                                   {:score score
                                    :name name
                                    :sign sign
                                    }))

(defn make-move [player board]
      (let [not-end (atom true) input (atom nil) coords (atom [0 0 0 0])]
           (do
             (println "Player " (:name player) " is inputting x1, y1, x2, y2 respectively: ")
             (while (= @not-end true)
                    (do
                      (reset! input (read-line))
                      (swap! input #(clojure.string/split % #" "))
                      (swap! coords #(assoc % 0 (* 2 (dec (Integer/parseInt (get @input 0))))))
                      (swap! coords #(assoc % 1 (* 2 (dec (Integer/parseInt (get @input 1))))))
                      (swap! coords #(assoc % 2 (* 2 (dec (Integer/parseInt (get @input 2))))))
                      (swap! coords #(assoc % 3 (* 2 (dec (Integer/parseInt (get @input 3))))))
                      (cond
                        (contains?
                          @(:used-moves board)
                          (Stick
                            (Point (get @coords 0) (get @coords 1))
                            (Point (get @coords 2) (get @coords 3))))
                        (println "This move has already been.")
                        :else
                        (do
                          (reset! not-end false)
                          (swap!
                            (:used-moves board)
                            #(conj % (Stick
                                       (Point (get @coords 0) (get @coords 1))
                                       (Point (get @coords 2) (get @coords 3)))))))))
             (cond
               (= (Math/abs (- (get @coords 1) (get @coords 3))) 2)
               (swap! (:configuration board) #(assoc %
                                                     (dec (Math/max (get @coords 1) (get @coords 3)))
                                                     (assoc
                                                       (get % (dec (Math/max (get @coords 1) (get @coords 3))))
                                                       (get @coords 0)
                                                       "|")))
               :else
               (swap! (:configuration board) #(assoc %
                                                     (get @coords 1)
                                                     (assoc
                                                       (get % (get @coords 1))
                                                       (inc (Math/min (get @coords 0) (get @coords 2)))
                                                       "_"))))
             @coords)))

(defmacro if-then [board current-bonus players whose-turn a b c d e f g h i j]
          `(if
             (and
               (= (get (get @(:configuration ~board) ~a) ~b) "_")
               (= (get (get @(:configuration ~board) ~c) ~d) "_")
               (= (get (get @(:configuration ~board) ~e) ~f) "|")
               (= (get (get @(:configuration ~board) ~g) ~h) "|"))
             (do
               (swap! ~current-bonus inc)
               (swap! (:score (get ~players @~whose-turn)) inc)
               (swap! (:configuration ~board) #(assoc %
                                                      ~i
                                                      (assoc
                                                        (get % ~i)
                                                        ~j
                                                        (:sign (get ~players @~whose-turn))))))))

(defn play [board players]
      (let [limit (+ (* (:x board) (inc (:y board))) (* (:y board) (inc (:x board))))
            current-bonus (atom 0) stick-counter (atom 0) whose-turn (atom 0)
            current-move (atom []) x1 (atom 0) y1 (atom 0) x2 (atom 0) y2 (atom 0)
            signal (atom true) winner-or-draw (atom []) max-score (atom 0) i (atom 0)]
           (do
             (while (not= @stick-counter limit)
                    (do
                      (reset! signal true)
                      (reset! current-move (make-move (get players @whose-turn) board))
                      (swap! stick-counter inc)
                      (if
                        (pos? @current-bonus)
                        (swap! current-bonus dec))
                      (if
                        (not= (get @current-move 0) (get @current-move 2))
                        (do
                          (reset! x1 (Math/min (get @current-move 0) (get @current-move 2)))
                          (reset! x2 (Math/max (get @current-move 0) (get @current-move 2)))
                          (reset! y1 (get @current-move 1))
                          (reset! y2 (get @current-move 1)))
                        (do
                          (reset! y1 (Math/min (get @current-move 1) (get @current-move 3)))
                          (reset! y2 (Math/max (get @current-move 1) (get @current-move 3)))
                          (reset! x1 (get @current-move 0))
                          (reset! x2 (get @current-move 0))))
                      (if
                        (and (= @y1 0) (= @y2 0))
                        (do
                          (reset! signal false)
                          (if-then board current-bonus players whose-turn 0 (dec @x2) 2 (dec @x2) 1 @x2 1 (- @x2 2) 1 @x2)))
                      (if
                        (and (= @y1 (* 2 (:y board))) (= @y1 @y2))
                        (do
                          (reset! signal false)
                          (if-then board current-bonus players whose-turn (- @y2 2) (dec @x2) @y2 (dec @x2) (dec @y2)
                                   @x2 (dec @y2) (- @x2 2) (dec @y2) (dec @x2))))
                      (if
                        (and (= @x1 0) (= @x1 @x2))
                        (do
                          (reset! signal false)
                          (if-then board current-bonus players whose-turn (- @y2 2) (inc @x2) @y2 (inc @x2) (dec @y2)
                                   @x2 (dec @y2) (+ @x2 2) (dec @y2) (inc @x2))))
                      (if
                        (and (= @x1 (* 2 (:x board))) (= @x1 @x2))
                        (do
                          (reset! signal false)
                          (if-then board current-bonus players whose-turn (- @y2 2) (dec @x2) @y2 (dec @x2) (dec @y2)
                                   @x2 (dec @y2) (- @x2 2) (dec @y2) (dec @x2))))
                      (if
                        (and (= @x1 @x2) (= @signal true))
                        (do
                          (if-then board current-bonus players whose-turn (- @y2 2) (dec @x2) @y2 (dec @x2) (dec @y2)
                                   @x2 (dec @y2) (- @x2 2) (dec @y2) (dec @x2))
                          (if-then board current-bonus players whose-turn (- @y2 2) (inc @x2) @y2 (inc @x2) (dec @y2)
                                   @x2 (dec @y2) (+ @x2 2) (dec @y2) (inc @x2))))
                      (if
                        (and (= @y1 @y2) (= @signal true))
                        (do
                          (if-then board current-bonus players whose-turn (- @y2 2) (dec @x2) @y2 (dec @x2) (dec @y2)
                                   @x2 (dec @y2) (- @x2 2) (dec @y2) (dec @x2))
                          (if-then board current-bonus players whose-turn @y2 (dec @x2) (+ @y2 2) (dec @x2) (inc @y2)
                                   @x2 (inc @y2) (- @x2 2) (inc @y2) (dec @x2))))
                      ((:print-board board))
                      (if
                        (zero? @current-bonus)
                        (reset! whose-turn (rem (inc @whose-turn) (count players))))))
             (reset! max-score (apply max (mapv #(deref %) (mapv #(:score %) players))))
             (while (< @i (count players))
                    (do
                      (if
                        (= @(:score (get players @i)) @max-score)
                        (swap! winner-or-draw #(conj % @i)))
                      (swap! i inc)))
             (if
               (= (count @winner-or-draw) 1)
               (println "Player " (:name (get players (first @winner-or-draw))) " has won.")
               (println "No one won."))
             (mapv #(reset! (:score %) 0) players)
             nil)))

(def player1 (HumanPlayer "Paul" "A"))
(def player2 (HumanPlayer "Peter" "B"))
(def board (Board 2 2))
(play board [player1 player2])

;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 1 1 2 1
;; .  _  .     .     .
;; 
;; .     .     .     .
;; 
;; .     .     .     .
;; 
;; .     .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 2 1 2 2
;; .  _  .     .     .
;;       |
;; .     .     .     .
;; 
;; .     .     .     .
;; 
;; .     .     .     .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 2 2 3 2
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;; 
;; .     .     .     .
;; 
;; .     .     .     .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 3 2 3 3
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;             |
;; .     .     .     .
;; 
;; .     .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 3 3 4 3
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;             |
;; .     .     .  _  .
;; 
;; .     .     .     .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 4 4 4 3
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;             |
;; .     .     .  _  .
;;                   |
;; .     .     .     .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 1 4 2 4
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;             |
;; .     .     .  _  .
;;                   |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 2 4 2 3
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;             |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 2 3 2 2
;; .  _  .     .     .
;;       |
;; .     .  _  .     .
;;       |     |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 2 1 3 1
;; .  _  .  _  .     .
;;       |
;; .     .  _  .     .
;;       |     |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 4 1 3 1
;; .  _  .  _  .  _  .
;;       |
;; .     .  _  .     .
;;       |     |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 4 1 4 2
;; .  _  .  _  .  _  .
;;       |           |
;; .     .  _  .     .
;;       |     |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 4 2 4 3
;; .  _  .  _  .  _  .
;;       |           |
;; .     .  _  .     .
;;       |     |     |
;; .     .     .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 2 3 3 3
;; .  _  .  _  .  _  .
;;       |           |
;; .     .  _  .     .
;;       |  B  |     |
;; .     .  _  .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 3 2 4 2
;; .  _  .  _  .  _  .
;;       |           |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 3 1 3 2
;; .  _  .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |           |
;; .  _  .     .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 2 4 3 4
;; .  _  .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |           |
;; .  _  .  _  .     .
;; Player  Peter  is inputting x1, y1, x2, y2 respectively:
;; 3 4 4 4
;; .  _  .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |           |
;; .  _  .  _  .  _  .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 3 4 3 3
;; .  _  .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 1 1 1 2
;; .  _  .  _  .  _  .
;; |     |  B  |  B  |
;; .     .  _  .  _  .
;;       |  B  |  B  |
;; .     .  _  .  _  .
;;       |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Michael  is inputting x1, y1, x2, y2 respectively:
;; 1 2 1 3
;; .  _  .  _  .  _  .
;; |     |  B  |  B  |
;; .     .  _  .  _  .
;; |     |  B  |  B  |
;; .     .  _  .  _  .
;;       |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 1 2 2 2
;; .  _  .  _  .  _  .
;; |  A  |  B  |  B  |
;; .  _  .  _  .  _  .
;; |     |  B  |  B  |
;; .     .  _  .  _  .
;;       |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 1 3 2 3
;; .  _  .  _  .  _  .
;; |  A  |  B  |  B  |
;; .  _  .  _  .  _  .
;; |  A  |  B  |  B  |
;; .  _  .  _  .  _  .
;;       |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Paul  is inputting x1, y1, x2, y2 respectively:
;; 1 4 1 3
;; .  _  .  _  .  _  .
;; |  A  |  B  |  B  |
;; .  _  .  _  .  _  .
;; |  A  |  B  |  B  |
;; .  _  .  _  .  _  .
;; |  A  |  C  |  C  |
;; .  _  .  _  .  _  .
;; Player  Peter  has won.
;; nil
Process finished with exit code 0
