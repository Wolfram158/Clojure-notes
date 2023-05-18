(defn get-numeric-vectors [& vectors] 
  (filter #(every? number? %) vectors))

(defn get-max-len [& vectors] 
  (apply max (mapv #(count %) (apply get-numeric-vectors vectors))))

(defn get-vectors-of-max-len [& vectors] 
  (let [max-len (apply get-max-len vectors)] 
    (filter #(= max-len (count %)) (apply get-numeric-vectors vectors))))

(defn get-min-product [& vectors] 
  (apply min (mapv #(apply * %) (apply get-vectors-of-max-len vectors))))
