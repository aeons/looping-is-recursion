(ns looping-is-recursion)

(defn power [base exp]
  (let [f (fn [acc n]
            (if (zero? n)
              acc
              (recur (* acc base) (dec n))))]
    (f 1 exp)))

(defn last-element [a-seq]
  (let [f (fn [s]
            (if (nil? (next s))
              (first s)
              (recur (rest s))))]
    (if (empty? a-seq)
      nil
      (f a-seq))))

(defn seq= [seq1 seq2]
  (let [f (fn [a b]
            (cond
             (and (empty? a) (empty? b)) true
             (or (empty? a) (empty? b)) false
             (not= (first a) (first b)) false
             :else (recur (rest a) (rest b))))]
    (f seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         i 0]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (rest s) (inc i)))))


(defn avg [a-seq]
  (loop [sum 0
         n 0
         s a-seq]
    (if (empty? s)
      (if (zero? n)
        0
        (/ sum n))
      (recur (+ sum (first s))
             (inc n)
             (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [set #{}
         seq a-seq]
    (if (empty? seq)
      set
      (recur (toggle set (first seq))
             (rest seq)))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [n n
           fn 1
           fn1 0]
      (if (= 1 n)
        fn
        (recur (dec n)
               (+ fn fn1)
               fn)))))


(defn cut-at-repetition [a-seq]
  (loop [res []
         rep #{}
         s a-seq]
    (if (empty? s)
      res
      (let [fs (first s)]
        (if (contains? rep fs)
          res
          (recur (conj res fs)
                 (conj rep fs)
                 (rest s)))))))
