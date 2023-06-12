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
