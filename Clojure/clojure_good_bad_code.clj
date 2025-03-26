;; Good Code
(defn good-sum [a b] (+ a b))

(defn good-factorial [n]
  (if (<= n 1)
    1
    (* n (good-factorial (- n 1)))))

(defn good-map-example [coll]
  (map inc coll))


;; Bad Code
(defn bad-sum [& args]
  (reduce + args))

(defn bad-factorial [n]
  (loop [i n acc 1]
    (if (<= i 1)
      acc
      (recur (- i 1) (* acc i)))))

(defn bad-map-example [coll]
  (let [result []]
    (doseq [x coll]
      (conj! result (inc x)))
    result))
