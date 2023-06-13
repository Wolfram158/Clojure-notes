(defn vec-remove [pos coll] (into (subvec coll 0 pos) (subvec coll (inc pos))))
(defn det [matrix] (cond (= 2 (count matrix)) (- 
                                                 (* 
                                                 (get (get matrix 0) 0) 
                                                 (get (get matrix 1) 1)) 
                                                      (* 
                                                      (get (get matrix 0) 1) 
                                                      (get (get matrix 1) 0))) 
                          :else (apply + 
                                       (for [i (range (count matrix))] (* 
                                                                       (Math/pow -1 i) 
                                                                       (get (get matrix 0) i) 
                                                                       (det (subvec (mapv (partial vec-remove i) matrix) 1)))))))
(defn almost-inverse [matrix] (cond (= 1 (count matrix)) (/
                                                           1 
                                                           (first (first matrix))) 
                                     :else (vec (for [i (range (count matrix))] 
                                              (vec (for [j (range (count matrix))] (*
                                                                                    (int (Math/pow -1 (+ i j))) 
                                                                                    (det (vec-remove i (mapv (partial vec-remove j) matrix))))))))))
(defn divide-vector [number vect] (mapv #(/ % number) vect))
(defn divide-matrix [number matrix] (mapv (partial divide-vector number) matrix))                                                                                                                                                                  
(defn inverse [matrix] (let [determinant (det matrix)] (apply mapv vector ((partial divide-matrix determinant) (almost-inverse matrix)))))
