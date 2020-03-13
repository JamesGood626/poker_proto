(ns poker-proto.core)

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

(def suits #{:spade :heart :club :diamond})
(def values #{:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A})

; The keys of the :players map correspond to seat-position, to facilitate updating a particular player.
(def player-state {:players [{:name "name1" :seat-position :1 :hand [] :folded false :bet-size 0}
                            {:name "name2" :seat-position :2 :hand [] :folded false :bet-size 0}
                            {:name "name3" :seat-position :3 :hand [] :folded false :bet-size 0}
                            {:name "name4" :seat-position :4 :hand [] :folded false :bet-size 0}
                            {:name "name5" :seat-position :5 :hand [] :folded false :bet-size 0}
                            {:name "name6" :seat-position :6 :hand [] :folded false :bet-size 0}
                            {:name "name7" :seat-position :7 :hand [] :folded false :bet-size 0}
                            {:name "name8" :seat-position :8 :hand [] :folded false :bet-size 0}
                            {:name "name9" :seat-position :9 :hand [] :folded false :bet-size 0}
                            {:name "name10" :seat-position :10 :hand [] :folded false :bet-size 0}]})

(def game-turn-state {:button-position :1
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

(def table-state {:player-state player-state
                  :game-turn-state game-turn-state
                  :card-state card-state
                  :bet-state bet-state})

; Keys in the player map will correspond to the player's
; seat-position, to facilitate retrieval when referenced
; from the table map.
(def players {:1 {:name "name1" :chips 10000 :hand [{:suit "Hearts" :card-value "J"} {:suit "Hearts" :card-value "10"}]}})

(defn make-card
  "
  Constructor for the Card type
  [:spade :A]
  "
  [suit value]
  [suit value])

; To generate a deck:
(defn build-deck [suits values]
  "Returns a deck of the shape:
  [[:spade :A] [:heart :Q] [:diamond :10]] ...and so on.
  But the return value is a list of four lists... so it still needs to be flattened.
  "
     (map (fn [suit] (map (fn [value] (make-card suit value)) values)) suits))

(defn flatten-deck [deck]
  "
  Flattening a single level of nested vectors:
  (reduce (fn [x acc] (concat x acc)) () ['(1) '(2) '(3)])
  result -> (1 2 3)
  "
  (reduce (fn [x acc] (concat x acc)) () deck))

(defn construct-deck [suits values]
  (flatten-deck (build-deck suits values)))

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
  "
  mid-point -> Int
  partition-point -> Int

  This function generates a random number to facilitate choosing whether
  the deck will be split below or above the mid-point.

  partition-point will always be less than or equal to the mid-point
  ensure the parition-point is within the possible range which the deck
  can be split.
  "
  (let [x (rand)]
    (if (< x 0.5)
      (- mid-point partition-point)
      (+ mid-point partition-point))))

(defn which-concat [l r]
  "
  Just another function to facilitate randomness in the shuffle after
  we've hit the base case and merge-cards is ran on the bubbled up values.
  "
  (let [x (rand)]
    (if (< x 0.5)
      (concat l r)
      (concat r l))))

; Then for merge-cards... you'll want to introduce some random
; generator in here as well for determining whether to prepend
; or append l w/ r.
(defn merge-cards
  "
  where l-partition and r-partition
  are a subset of the deck which have been
  partitioned up to this point.
  "
  [l-partition r-partition]
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
  "
  (take 2 '(1 2 3 4)) # => (1 2)
  (drop 2 '(1 2 3 4)) # => (3 4)
  "
  (let [mid (get-mid-point deck-partition)
        randomized-split (split-up-or-down mid (gen-rand-int mid))
        l-partition (take randomized-split deck-partition)
        r-partition (drop randomized-split deck-partition)]
    (if (<= (count deck-partition) 2)
      (merge-cards l-partition r-partition)
      (merge-cards (mix-cards l-partition) (mix-cards r-partition)))))

(defn shuffle [deck n]
  "
  The power of recursion!
  "
  (if (= n 0)
    deck
    (shuffle (mix-cards deck) (- n 1))))

; (def deck (flatten-deck (build-deck suits card-values)))
; (def shuffled-deck (shuffle deck 1000)) => gives a pretty solid mix up of the deck

; Accessor function
(defn player-hand [player]
  (:hand player))

(defn add-card-to-hand [player card]
  "
  pp/deal depends on this function.

  Refactored what this function used to call
  from this:
  (assoc player :hand (conj (:hand player) card)

  To the new version which uses the accessor function
  to get at the player's hand incase the data structure
  needs to change in the future.
  "
  (assoc player :hand (conj (player-hand player) card)))

; Would be ideal to use destructuring instead.
(defn deal [deck player]
  "
  pp/update-player-hand depends on this function.
  "
  (let [card (first deck)
        remaining-deck (rest deck)]
    {:remaining-deck remaining-deck :updated-player (add-card-to-hand player card)}))


(def players [{:name "name1" :seat-position :1 :hand [] :folded false :bet-size 0} {:name "name2" :seat-position :3 :hand [] :folded false :bet-size 0}])

(defn seat-position-numeric-value [seat-position]
  (case seat-position
    :1 1
    :2 2
    :3 3
    :4 4
    :5 5
    :6 6
    :7 7
    :8 8
    :9 9
    :10 10))

(defn players [player-state]
  (:players player-state))

(defn update-player-in-players [player-state player-pos-in-list updated-player]
  (println "player-pos-in-list")
  (println player-pos-in-list)
  (assoc-in (players player-state) [player-pos-in-list] updated-player))
  ; (let [player-pos-in-list (- (seat-position-numeric-value seat-position) 1)])

(defn update-player-hand
  "
  pp/deal-hand depends on this function.

  updated-player-state transformation:
  From:
  {:players {:1 {:name 'name1', :seat-position :1, :hand [], :folded false, :bet-size 0}, :3 {:name 'name2', :seat-position :3, :hand [], :folded false, :bet-size 0}}}
  To:
  (assoc-in player-state [:players :1] 'THIS SHIT WORKED')
  {:players {:1 'THIS SHIT WORKED', :3 {:name 'name2', :seat-position :3, :hand [], :folded false, :bet-size 0}}}

  Ultimately, this function returns a data structure of this shape:

  {:remaining-deck
    ([:heart :A] [:club :9] [:heart :7] [:spade :10] [:diamond :3] [:diamond :A] [:heart :8] [:spade :9] [:diamond :2] [:spade :A] [:heart :4] [:diamond :4] [:spade :2]
    [:spade :Q] [:club :6] [:heart :K] [:club :2] [:diamond :7] [:spade :5] [:club :J] [:club :5] [:spade :3] [:club :K] [:club :7] [:diamond :J] [:heart :5] [:diamond :6]
    [:spade :J] [:diamond :8] [:heart :Q] [:heart :9] [:spade :4] [:diamond :5] [:heart :J] [:spade :K] [:club :8] [:club :10] [:spade :8] [:spade :7] [:heart :2] [:club :3]
    [:club :4] [:club :Q] [:diamond :10] [:heart :3] [:heart :10] [:heart :6] [:club :A] [:diamond :Q] [:spade :6] [:diamond :9]),
   ; The money shot.
   :updated-player-state {:players {:1 {:name 'name1', :seat-position :1, :hand [[:diamond :K]], :folded false, :bet-size 0}}}}

   For updating a player in the acc vector:
   (assoc-in list [0] 10) ; => [10 2 3 4]
   (assoc-in list [1] 10) ; => [1 10 3 4]
  "
  [acc deck player]
  (let [result (deal deck player)
        updated-player (:updated-player result)
        seat-position (:seat-position player)
        remaining-deck (:remaining-deck result)
        updated-player-state (update-player-in-players acc (.indexOf (players acc) player) updated-player)]
      ;  (println {:remaining-deck remaining-deck :updated-player-state updated-player-state})
      (println "The updated-player-state")
      (println updated-player-state)
      (println "The acc")
      (println acc)
  {:remaining-deck remaining-deck :players updated-player-state}))

; (defn get-first-player [])

(defn deal-hand
  "
  pp/deal-initial-hands depends on this function.

  NOTE:
  unfucked-player is necessary because reduce converts each element
  in the map into a vector of 2 elements:
    [:1 {:name 'name1', :seat-position :1, :hand [], :folded false, :bet-size 0}]
  We convert it back into a map because that's the format the dependent functions are expecting:
    {:name 'name1', :seat-position :1, :hand [], :folded false, :bet-size 0}
  "
  [deck]
  (fn [acc player]
    ; (def unfucked-player (nth player 1))
    (def use-deck (if (= (:remaining-deck acc) nil)
                      deck
                      (:remaining-deck acc)))
                      (println "The player")
                      (println player)
    (update-player-hand acc use-deck player)))

; CLOSE BUT SMALL ERRORS IN THE updated-player-state
; {:remaining-deck
;   ([:diamond :6] [:heart :Q] [:heart :2] [:heart :4] [:spade :5] [:heart :7] [:spade :10] [:heart :3]
;    [:club :A] [:club :7] [:diamond :J] [:heart :J] [:club :K] [:diamond :Q] [:club :2] [:club :10]
;    [:club :9] [:spade :A] [:spade :K] [:heart :8] [:heart :A] [:diamond :4] [:diamond :2] [:heart :5]
;    [:spade :2] [:heart :K] [:club :6] [:heart :6] [:diamond :10] [:spade :Q] [:diamond :5] [:club :4]
;    [:spade :8] [:club :Q] [:spade :9] [:diamond :7] [:spade :3] [:club :J] [:club :3] [:diamond :8]
;    [:diamond :9] [:heart :10] [:heart :9] [:diamond :3] [:spade :J] [:club :8] [:spade :4] [:spade :7]
;    [:club :5] [:diamond :A]), :updated-player-state {:remaining-deck ([:spade :6] [:diamond :6] [:heart :Q]
;    [:heart :2] [:heart :4] [:spade :5] [:heart :7] [:spade :10] [:heart :3] [:club :A] [:club :7] [:diamond :J]
;    [:heart :A] [:diamond :4] [:diamond :2] [:heart :5] [:spade :2] [:heart :K] [:club :6] [:heart :6] [:diamond :10] [:diamond :A]),
;    [:heart :J] [:club :K] [:diamond :Q] [:club :2] [:club :10] [:club :9] [:spade :A] [:spade :K] [:heart :8]
;   :updated-player-state {:players
;       {nil {:1 {:name "name1", :seat-position :1, :hand [], :folded false, :bet-size 0}, :hand ([:diamond :K])}}},
;       :players {nil {:3 {:name "name2", :seat-position :3, :hand [], :folded false, :bet-size 0}, :hand ([:spade :6])}}}}

; The culprit is update-player-hand:
; {:remaining-deck ([:heart :A] [:club :9] [:heart :7] [:spade :10] [:diamond :3] [:diamond :A] [:heart :8] [:spade :9]
; [:diamond :2] [:spade :A] [:heart :4] [:diamond :4] [:spade :2] [:spade :Q] [:club :6] [:heart :K] [:club :2] [:diamond :7]
; [:spade :5] [:club :J] [:club :5] [:spade :3] [:club :K] [:club :7] [:diamond :J] [:heart :5] [:diamond :6] [:spade :J] [:diamond :8]
; [:heart :Q] [:heart :9] [:spade :4] [:diamond :5] [:heart :J] [:spade :K] [:club :8] [:club :10] [:spade :8] [:spade :7] [:heart :2]
; [:club :3] [:club :4] [:club :Q] [:diamond :10] [:heart :3] [:heart :10] [:heart :6] [:club :A] [:diamond :Q] [:spade :6] [:diamond :9]),
; THE OFFENDING LINE:
; :updated-player-state {:players {nil {:1 {:name "name1", :seat-position :1, :hand [], :folded false, :bet-size 0}, :hand ([:diamond :K])}}}}

(defn button-position [game-state]
  (:button-position game-state))

(defn adjust-players-position-for-dealing
  "
  First card is dealt to the player immediately following the button.

  So with an input of
  players -> vector containing maps of player state
  button-position -> 3

  The output is:
  [{:name 'name4', :seat-position :4, :hand [], :folded false, :bet-size 0}
  {:name 'name5', :seat-position :5, :hand [], :folded false, :bet-size 0}
  {:name 'name6', :seat-position :6, :hand [], :folded false, :bet-size 0}
  {:name 'name7', :seat-position :7, :hand [], :folded false, :bet-size 0}
  {:name 'name8', :seat-position :8, :hand [], :folded false, :bet-size 0}
  {:name 'name9', :seat-position :9, :hand [], :folded false, :bet-size 0}
  {:name 'name10', :seat-position :10, :hand [], :folded false, :bet-size 0}
  {:name 'name1', :seat-position :1, :hand [], :folded false, :bet-size 0}
  {:name 'name2', :seat-position :2, :hand [], :folded false, :bet-size 0}
  {:name 'name3', :seat-position :3, :hand [], :folded false, :bet-size 0}]
  "
  [players button-position]
    (let [adjusted-position button-position]
      (->
        (concat (drop adjusted-position players) (take adjusted-position players))
        vec)))

; TODO: add button-position arg
(defn deal-single-round
  "
  Deals out a single card to each player.
  
  let:
  n = current_button_position - 1

  So if
  current_button_position = 3
  n = 3 - 1 = 2

  (def players [1 2 3 4 5 6 7 8 9 10])
  (concat (drop n players) (take n players))
  (3 4 5 6 7 8 9 10 1 2)

  This will allow us to cycle through to rotate rounds based on button position...
  But these functions will now need to be modified to accomodate taking a button-position as an arg.
  "
  [deck players game-turn-state]
  (let [players-ordered-by-turn (adjust-players-position-for-dealing players (button-position game-turn-state))]
        (println "The players-ordered-by-turn")
        (println players-ordered-by-turn)
  (reduce (deal-hand deck) {:remaining-deck nil :players players-ordered-by-turn} players-ordered-by-turn)))

(defn deal-initial-hands [deck players n-cards]
  (let [single-round-dealt-result (deal-single-round deck players)]
    ; (println "The single-round-dealt-result")
    ; (println single-round-dealt-result)
    (if (= n-cards 0)
      single-round-dealt-result
      (deal-initial-hands (:deck single-round-dealt-result) (:players single-round-dealt-result) (- n-cards 1)))))

; IMMEDIATE TODO:
; The results from deal-single-round are coming out unordered... 
; {:10 {:name "name10", :seat-position :10, :hand [[:spade :5]], :folded false, :bet-size 0},
;   :4 {:name "name4", :seat-position :4, :hand [[:club :8]], :folded false, :bet-size 0},
;   :7 {:name "name7", :seat-position :7, :hand [[:club :Q]], :folded false, :bet-size 0}, :1 {:name "name1", :seat-position :1, :hand [[:heart :9]], :folded false, :bet-size 0}, :8 {:name "name8", :seat-position :8, :hand [[:heart :10]], :folded false, :bet-size 0}, :9 {:name "name9", :seat-position :9, :hand [[:diamond :J]], :folded false, :bet-size 0}, :2 {:name "name2", :seat-position :2, :hand [[:club :5]], :folded false, :bet-size 0}, :5 {:name "name5", :seat-position :5, :hand [[:club :2]], :folded false, :bet-size 0}, :3 {:name "name3", :seat-position :3, :hand [[:spade :9]], :folded false, :bet-size 0}, :6 {:name "name6", :seat-position :6, :hand [[:heart :4]], :folded false, :bet-size 0}}