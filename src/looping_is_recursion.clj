(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base acc exp]
    (if (= exp 0)
      acc
      (recur base (* base acc) (- exp 1))))]
    (helper base 1 exp) ))


(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq)) ) )

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) (empty? seq2)
   (empty? seq2) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false ) )

(defn find-first-index [pred a-seq]
  (loop [acc 0, b-seq a-seq]
    (cond
     (empty? b-seq) nil
     (pred (first b-seq)) acc
     :else (recur (inc acc) (rest b-seq)) ) ) )

(defn avg [a-seq]
  (loop [sum 0, n 0, b-seq a-seq]
    (if (empty? b-seq)
      (/ sum n)
      (recur (+ sum (first b-seq)) (inc n) (rest b-seq)) ) ) )

(defn parity [a-seq]
  (loop [a-set #{}, b-seq a-seq]
    (cond
     (empty? b-seq) a-set
     (some #(= (first b-seq) %) a-set) (recur (disj a-set (first b-seq)) (rest b-seq))
     :else (recur (conj a-set (first b-seq)) (rest b-seq)) ) ) )

(defn fast-fibo [n]
  (loop [a 0, b 1, nn n]
    (if (zero? nn)
     a
     (recur b (+ a b) (dec nn)) ) ) )

(defn cut-at-repetition [a-seq]
  (loop [seen #{}, n 0, b-seq a-seq]
    (cond
      (empty? b-seq)  (take n a-seq)
      (some #(= (first b-seq) %) seen)  (take n a-seq)
      :else (recur (conj seen (first b-seq)) (inc n) (rest b-seq)) ) ) )
