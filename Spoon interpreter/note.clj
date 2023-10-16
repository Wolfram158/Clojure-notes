(require '[clojure.string :refer [starts-with?]])

(defn test [s word] (starts-with? s word))

(defn parse-abstract [word res s] (if (test s word) [res (subs s (count word))] [nil s]))

(def parse-inc (partial parse-abstract "1" "+"))
(def parse-dec (partial parse-abstract "000" "-"))
(def parse-next (partial parse-abstract "010" ">"))
(def parse-prev (partial parse-abstract "011" "<"))
(def parse-cycle-begin (partial parse-abstract "00100" "["))
(def parse-cycle-end (partial parse-abstract "0011" "]"))
(def parse-input (partial parse-abstract "0010110" ","))
(def parse-print (partial parse-abstract "001010" "."))

(defn map-all [s] (map #(% s) [parse-inc parse-dec parse-next parse-prev parse-cycle-begin
                               parse-cycle-end parse-input parse-print]))

(defn spoonToBFImpl [cur rest] (let [parsed (filter #(not= (first %) nil) (map-all rest))]
   (cond
         (= rest "")
         cur
         (= parsed ())
         (println "Error")
         :else
         (spoonToBFImpl (apply str cur (first (first parsed))) (second (first parsed))))))
