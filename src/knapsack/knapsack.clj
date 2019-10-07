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

(defn make-population
  [num-individuals num-items]
  (repeatedly num-individuals
              #(make-random-solution num-items)))

(defn summarize-individual
  [problem individual]
  (let [items (:items problem)
        values (map :value items)
        weights (map :weight items)
        total-value (reduce + (map * values individual))
        total-weight (reduce + (map * weights individual))]
    {:total-value total-value
     :total-weight total-weight}))

(defn ethan-hamers-fitness
  [problem individual]
  (let [summary (summarize-individual problem individual)]
    (/ (:total-value summary)
       (+ 1 (Math/abs (- (:total-weight summary)
                         (:max-capacity problem)))))))

(defn tournament-selection
  [problem tournament-size popl]
  (let [tournament (take tournament-size (shuffle popl))
        fitnesses (map (partial ethan-hamers-fitness problem) tournament)
        pairs (map vector tournament fitnesses)
        max-fitness (apply max fitnesses)]
    (first (first (filter #(= max-fitness (second %)) pairs)))))

(defn select-parents
  [problem tournament-size popl]
  (let [pop-size (count popl)]
    (repeatedly pop-size #(tournament-selection problem tournament-size popl))))
