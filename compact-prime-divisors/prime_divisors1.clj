(defn get-divisors-bounded-sqrt [x] 
  (filter 
   #(= (rem x %) 0) 
   (range 2 (inc (Math/ceil (Math/sqrt x))))))

(defn is-prime [x] 
  (cond (= x 1) false (= x 2) true 
    :else (= 
           (dec (int (Math/floor (Math/sqrt x)))) 
           (count (filter #(not= (rem x %) 0) (range 2 (Math/ceil (Math/sqrt x))))))))

(defn get-prime-divisors [x] 
  (let [divisors-bounded-sqrt (get-divisors-bounded-sqrt x)] 
    (cond (is-prime x) [x] :else (distinct 
                                  (filter 
                                   #(is-prime %) 
                                   (apply conj 
                                          (vec divisors-bounded-sqrt) 
                                          (reverse (mapv #(/ x %) divisors-bounded-sqrt))))))))

(defn get-power [a b] 
  (cond (= (rem a b) 0) (inc (get-power (int (Math/floor (/ a b))) b)) :else 0))

(defn compact-prime-divisors [x] 
  (let [prime-divisors (get-prime-divisors x)] 
    (mapv vector (vec prime-divisors) (mapv (partial get-power x) prime-divisors))))
