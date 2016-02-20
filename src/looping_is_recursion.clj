(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc k]
          (if (= 0 k)
            acc
            (recur (* acc base) (dec k))))]
    (helper 1 exp)))

  (defn last-element [a-seq]
    (let [helper
          (fn [coll]
            (if (= (count coll) 1)
              (first coll)
              (recur (rest coll))))]
      (if (empty? a-seq)
        nil
        (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [s1 s2]
          (cond (not (= (first s1) (first s2)))
                false
                (and (empty? s1) (empty? s2))
                true
                :else
                (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ind 0 sq a-seq]
    (cond (empty? sq)
          nil
          (pred (first sq))
          ind
          :else
          (recur (inc ind) (rest sq)))))

(defn avg [a-seq]
  (loop [sq a-seq acc 0]
    (if (empty? sq)
      (/ acc (count a-seq))
      (recur (rest sq) (+ acc (first sq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [tracker #{} sq a-seq]
    (if (empty? sq)
      tracker
      (recur (toggle tracker (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (cond (= n 0)
        0
        (< n 2)
        1
        :else
        (loop [fib-prev 1
               fib-n 1
               nth 2]
          (if (= n nth)
            fib-n
            (recur fib-n (+ fib-n fib-prev) (inc nth))))))

(defn cut-at-repetition [a-seq]
  (loop [n 1
         sq (take 1 a-seq)
         seen {}]
    (cond
      (get seen (last sq))
      (take (dec n) a-seq)
      (= sq a-seq)
      a-seq
      :else
      (recur (inc n) (take (inc n) a-seq) (assoc seen (last sq) true)))))


