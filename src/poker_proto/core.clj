(ns poker-proto.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

; 4 suits:
; [:spade :suit :heart :club :diamond]
; 13 values:
; [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A]
; A Hand:
; [[suit card-value] [suit card-value]]
; Table Cards
; {:flop [
;   [suit card-value]
;   [suit card-value]
;   [suit card-value]
;  ]
;  :turn [[suit card-value]]
;  :river [[suit card-value]]
; }
; OR:
; [[suit card-value]... repeat 5 times]

(def suits [:spade :suit :heart :club :diamond])
(def card-values [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])

(def player-state {:players [{:name "name1" :seat-position 1 :hand [] :folded false :bet-size 0} {:name "name2" :seat-position 3 :hand [] :folded false :bet-size 0}]})

(def game-turn-state {:big-blind-position 3
                      :small-blind-position 1
                      :player-turn 1})

(def card-state {:cards [{:suit "Hearts" :card-value "A"} {:suit "Hearts" :card-value "K"} {:suit "Hearts" :card-value "Q"}]
                 :card-turn :flop})

(def bet-state {; player-turn will be changed by using the length of :players
                ; :bet-history perhaps...
                :bet-action [:raise 30000] ; Used to display message to current player
                ; player-turn will run around the table
                ; and prompt each player for their call.
                ; If it's reraised, then that will be reflected
                ; in bet-action as :re-raise as well and the action will continue.
                :pot 200000})

(def table {:player-state player-state
            :game-turn-state game-turn-state
            :card-state card-state
            :bet-state bet-state})


; Keys in the player map will correspond to the player's
; seat-position, to facilitate retrieval when referenced
; from the table map.
(def players {:1 {:name "name1" :chips 10000 :hand [{:suit "Hearts" :card-value "J"} {:suit "Hearts" :card-value "10"}]}})

; To generate a deck:
(defn build-deck [suits card-values]
  (map (fn [suit] (map (fn [card-val] [suit card-val]) card-values)) suits))

; Flattening a single level of nested vectors:
; (reduce (fn [x acc] (concat x acc)) () ['(1) '(2) '(3)])
; result -> (1 2 3)

(defn flatten-deck [deck]
  (reduce (fn [x acc] (concat x acc)) () deck))

; (flatten-deck (build-deck suits card-values))
; yields:
; ([:spade :2] [:spade :3] [:spade :4] [:spade :5] [:spade :6] [:spade :7]
; [:spade :8] [:spade :9] [:spade :10] [:spade :J] [:spade :Q] [:spade :K]
; [:spade :A] [:suit :2] [:suit :3] [:suit :4] [:suit :5] [:suit :6] [:suit :7]
; [:suit :8] [:suit :9] [:suit :10] [:suit :J] [:suit :Q] [:suit :K] [:suit :A]
; [:heart :2] [:heart :3] [:heart :4] [:heart :5] [:heart :6] [:heart :7] [:heart :8]
; [:heart :9] [:heart :10] [:heart :J] [:heart :Q] [:heart :K] [:heart :A] [:club :2]
; [:club :3] [:club :4] [:club :5] [:club :6] [:club :7] [:club :8] [:club :9] [:club :10]
; [:club :J] [:club :Q] [:club :K] [:club :A] [:diamond :2] [:diamond :3] [:diamond :4] 
; [:diamond :5] [:diamond :6] [:diamond :7] [:diamond :8] [:diamond :9] [:diamond :10]
; [:diamond :J] [:diamond :Q] [:diamond :K] [:diamond :A])

; ---------------------------------------------------
; Started to attempt implementation of shuffling....
; ---------------------------------------------------
; Figured it'll be a recursive function that uses clojure's partition in some way.
; Couldn't figure out how to retrieve an element from the list from a particular index w/out the internet.

; NEVERMIND:
; (nth '(1 2 3) 0) => 1
; Now I need to figure out how to insert new values into a vector...
; Can use (into [] flat-deck) to convert the flat-deck list into a vector

(defn gen-rand-int [max-num]
  "Generates a random number from 1 through max-num"
  (int (Math/ceil (* (rand) max-num))))

(defn get-mid-point [xs]
  (int (/ (count xs) 2)))

(defn split-up-or-down [mid-point partition-point]
  (let [x (rand)]
    (if (< x 0.5)
      (- mid-point partition-point)
      (+ mid-point partition-point))))

(defn which-concat [l r]
  (let [x (rand)]
    (if (< x 0.5)
      (concat l r)
      (concat r l))))

; Then for merge... you'll want to introduce some random
; generator in here as well for determining whether to prepend
; or append l w/ r.
(defn merge [l-partition r-partition]
  (which-concat l-partition r-partition))

; Future fixes... as I'm calling it quits on this because
; VSCode is being a bitch.. I need to learn EMACS.
; But one concern is that the gen-rant-int function will outpput
; numbers from 1-10

; The below should be accounted for by the param max-num in gen-rand-int:
; if the deck-partition's mid is 5 because it's count is only 10
; and you add gen-rand-int of 8 to the mid in randomized-split... then
; you'll be out of range. So you need to add some code to account for smaller
; deck-partitions.

; NOTE:
; (take 2 '(1 2 3 4 5)) -> '(1 2)
; (drop 2 '(1 2 3 4 5)) -> '(3 4 5)
(defn mix-cards [deck-partition]
  (let [mid (get-mid-point deck-partition)
        randomized-split (split-up-or-down mid (gen-rand-int mid))
        l-partition (take randomized-split deck-partition)
        r-partition (drop randomized-split deck-partition)]
    (if (<= (count deck-partition) 2)
      (merge l-partition r-partition)
      (merge (mix-cards l-partition) (mix-cards r-partition)))))

; The power of recursion!
(defn shuffle [deck n]
  (if (= n 0)
    deck
    (shuffle (mix-cards deck) (- n 1))))

; (def deck (flatten-deck (build-deck suits card-values)))
; (def shuffled-deck (shuffle deck 1000)) => gives a pretty solid mix up of the deck

(defn add-card [card player]
  (assoc player :hand (conj (:hand player) card)))

; Would be ideal to use destructuring instead.
(defn deal [deck player]
  (let [card (first deck)
        remaining-deck (rest deck)]
    [remaining-deck (add-card card player)]))



(def players [{:name "name1" :seat-position 1 :hand [] :folded false :bet-size 0} {:name "name2" :seat-position 3 :hand [] :folded false :bet-size 0}])

; Getting -> Wrong number of args (0) passed to: clojure.lang.PersistentArrayMap
; when calling this function.
(defn deal-initial-hands [deck players n-cards]
  (let [helper-fn (fn [player acc]
                    (def result (deal (get acc :deck) player))
                    (def players-updated (assoc acc :players (conj (:players acc) (last result))))
                    ((assoc players-updated :deck (first result))))
        dealt-result (reduce helper-fn {:players [] :deck deck} players)]
    (if (= n-cards 0)
      dealt-result
      (deal-initial-hands (:deck dealt-result) (:players dealt-result) (- n-cards 1)))))