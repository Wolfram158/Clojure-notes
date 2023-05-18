(defn get-max-len [& vectors] 
  (apply max (mapv #(count %) vectors)))

(defn get-numeric-vectors-of-max-len [& vectors] 
  (filter #(and 
            (every? number? %) 
            (= (count %) (apply get-max-len vectors))) 
          vectors))

(defn get-min-product [& vectors] 
  (apply min (mapv #(apply * %) (vec (apply get-numeric-vectors-of-max-len vectors)))))
