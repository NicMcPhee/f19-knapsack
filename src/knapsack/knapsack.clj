(ns knapsack.knapsack)

; An item is a map with :weight
; and :value keys.
(defn make-random-item
  "Creates a random item whose weight is in
   the range [1, max-weight] and whose
   value is in the range [0, max-value-1]."
  [max-weight max-value]
  {:weight (inc (rand-int max-weight))
   :value (rand-int max-value)})

(defn make-random-problem
  [max-capacity num-items max-weight max-value]
  {:max-capacity max-capacity
   :items 
   (repeatedly 
    num-items 
    #(make-random-item max-weight max-value))})

(defn make-random-solution
  [num-items]
  (repeatedly num-items #(rand-int 2)))

(defn make-individual
  [generation bits]
  {:generation generation
   :bits bits})

(defn make-population
  [num-individuals num-items]
  (repeatedly num-individuals
              #(make-individual 0 (make-random-solution num-items))))

(defn summarize-individual
  [problem individual]
  (let [items (:items problem)
        values (map :value items)
        weights (map :weight items)
        total-value (reduce + (map * values (:bits individual)))
        total-weight (reduce + (map * weights (:bits individual)))]
    (merge individual
           {:total-value total-value
            :total-weight total-weight})))

(defn summarize-population
  [problem population]
  (map (partial summarize-individual problem) population))

(defn ethan-hamers-quality
  [problem individual]
  (/ (:total-value individual)
     (+ 1 (Math/abs (- (:total-weight individual)
                       (:max-capacity problem))))))

(defn harsh-quality
  [problem individual]
  (if (> (:total-weight individual)
         (:max-capacity problem))
    0
    (:total-value individual)))

(defn ramped-quality
  [problem individual]
  (if (> (:total-weight individual)
         (:max-capacity problem))
    (- (:total-weight individual))
    (:total-value individual)))

(defn evaluate-population
  [evaluator problem population]
  (map #(assoc % :quality (evaluator problem %)) 
       (summarize-population problem population)))

; Assumes popl has been evaluated, i.e., every individual has a
; `:quality` field.
(defn tournament-selection
  [tournament-size popl]
  (let [tournament (take tournament-size (shuffle popl))
        qualities (map :quality tournament)
        max-quality (apply max qualities)]
    (first (filter #(= max-quality (:quality %)) tournament))))

(defn select-parents
  [tournament-size popl]
  (let [pop-size (count popl)]
    (repeatedly pop-size #(tournament-selection tournament-size popl))))

(defn make-child-copy
  [parent]
  (assoc parent :generation (inc (:generation parent))))

(defn mutate-bit
  [mutation-rate bit]
  (if (< (rand) mutation-rate)
    (- 1 bit)
    bit))

(defn mutate-bits
  [mutation-rate bits]
  (map (partial mutate-bit mutation-rate) bits))

(defn make-child
  [mutation-rate parent]
  {:generation (inc (:generation parent))
   :bits (mutate-bits mutation-rate (:bits parent))})

(defn report
  [prob popl]
  (println (:generation (first popl))
           "\t" (* 1.0 (apply max (map :quality popl)))
           "\t" (apply max (map (partial harsh-quality prob)
                                popl))))

(def *print-report* false)

(defn next-generation
  [evaluator prob tournament-size popl]
  (let [evaluated-pop (evaluate-population evaluator prob popl)]
    (if *print-report*
      (report prob evaluated-pop))
    (map (partial make-child (/ 1 (count (:items prob))))
         (select-parents tournament-size evaluated-pop))))

(defn do-run
  [prob quality-fn tournament-size pop-size num-generations]
  (if *print-report*
    (println "Generation\tMax-quality\tMax-legal-value"))
  (let [num-items (count (:items prob))
        initial-pop (make-population pop-size num-items)
        final-pop (nth (iterate
                        (partial next-generation quality-fn prob tournament-size)
                        initial-pop)
                       num-generations)
        evaluated-pop (evaluate-population quality-fn prob final-pop)]
    (if *print-report*
      (report prob evaluated-pop))
    (apply max (map (partial harsh-quality prob) evaluated-pop))))

(defn do-experiment
  [num-runs prob quality-fn tournament-size pop-size num-generations]
  (repeatedly num-runs #(do-run prob quality-fn tournament-size pop-size num-generations)))

; (ks/do-run prob ks/ramped-quality 2 100 100)

; (def prob (ks/make-random-problem 50 10 25 25))
; (def popl (ks/make-population 100 10))
; (nth (iterate (partial ks/next-generation ks/ramped-quality prob 2) popl) 100)

