(ns poker-proto.core-test
  (:require [clojure.test :refer :all]
            [poker-proto.core :as pp]))
(use 'clojure.data)

(def proper-deck '(
  ; suits
  {:key :spade, :count 13} 
  {:key :heart, :count 13}
  {:key :diamond, :count 13}
  {:key :club, :count 13}
  ; values
  {:key :A, :count 4}
  {:key :K, :count 4}
  {:key :Q, :count 4}
  {:key :J, :count 4}
  {:key :10, :count 4}
  {:key :9, :count 4}
  {:key :8, :count 4}
  {:key :7, :count 4}
  {:key :6, :count 4}
  {:key :5, :count 4}
  {:key :4, :count 4}
  {:key :3, :count 4}
  {:key :2, :count 4}
))

; (def suits [:spade :suit :heart :club :diamond])
; (def values [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])

; Generally want to capture the properties of a deck...
; which should be that for each suit there is a frequency of 13.
; and for each card-value there is a frequency of 4.

; After finding this SO post: https://stackoverflow.com/a/25616080/10383131
; I modified the answer slightly to arrive at this helper function
; (defn count-occurences [term xs]
;   (->> xs
;        flatten
;        frequencies
;        term))
; REMEMBER: frequencies returns a map, so that term at the bottom of the trush
; is {term {:spade 2, :heart 1}} <- getting :spade from the map.
; (count-occurences :spade [[:spade :A] [:heart :10] [:spade :9]]) # => 2

; Now to modify it further so that a list of terms may be passed in.
; From the docs: https://clojuredocs.org/clojure.core/map
;; map can be used with multiple collections. Collections will be consumed
;; and passed to the mapping function in parallel:
; (map + [1 2 3] [4 5 6]) ...and the data structure maps are a collection
; So, (map terms result-of-frequencies) should work
; (map {:a 1 :b 2 :c 3} [:a :b :c]) # => (1 2 3) But the result-of-frequencies must
; be the first argument, so a curried function is required (see map-curried-terms-over-frequencies)!

; The result is w/ this implementation of count-occurences and map-curried-terms-over-frequencies:
; (defn map-curried-terms-over-frequencies [terms]
;   (fn [frequencies]
;     (map frequencies terms)))

; (defn count-occurences [terms xs]
;   (let [curried-map (map-curried-terms-over-frequencies terms)]
;     (->> xs
;          flatten
;          frequencies
;          curried-map)))

; RESULT:
; (count-occurences [:spade :heart] [[:spade :A] [:heart :10] [:spade :9] [:spade :2]])
; (3 1)
; Unfortunately that's still not enough, we'll need the returned list to include the key
; for which value is being checked for within the frequencies collection.
; So that we can test that each built deck contains exactly:
; (:spade 13 :heart 13 :A 4 :Q 4 :10 4 ....etc)

(defn map-curried-terms-over-frequencies [terms]
  (fn [frequencies]
    (map frequencies terms)))

(defn sub-divided-groups [deck] (group-by (fn [x] [(get x 0) (get x 1)] deck)))

; Deprecated, only for reference
(defn count-occurences-dep [terms]
  (let [curried-map (map-curried-terms-over-frequencies terms)]
    "
    NOTE:
    if I place (map-curried-terms-over-frequencies terms)
    where curried-map is, I get an error, but with let and curried-map
    assigned to that value, it's no issue.

    Example:
    (count-occurences [:spade :heart] [[:spade :A] [:heart :10] [:spade :9] [:spade :2]])
    (3 1)

    To facilitate a better output rather than (3 1) NOTE: This approach requires a rewrite of count-occurences to implement
    the below behavior.
    (def partial-deck [[:spade :A] [:heart :10] [:spade :9] [:spade :2]]) 
    1. (def groups (group-by (fn [x] (get x 0)) partial-deck)) # => {:spade [[:spade :A] [:spade :9] [:spade :2]], :heart [[:heart :10]]}
    # NOTE: This group-by approach clearly fails to get the values.... Needed to add more
    (def groups (group-by (fn [x] [(get x 0) (get x 1)]) partial-deck)) # => {[:spade :A] [[:spade :A]], [:heart :10] [[:heart :10]], [:spade :9] [[:spade :9]], [:spade :2] [[:spade :2]]}
    (vec groups) # => [[[:spade :A] [[:spade :A]]] [[:heart :10] [[:heart :10]]] [[:spade :9] [[:spade :9]]] [[:spade :2] [[:spade :2]]]]
    (flatten (vec groups)) # => (:spade :A :spade :A :heart :10 :heart :10 :spade :9 :spade :9 :spade :2 :spade :2)
    (set (flatten (vec groups))) # => #{:spade :heart :A :10 :9 :2} ; And sets are able to be mapped over
    2. (def group_keys (keys groups)) # => (:spade :heart)
    3. (map (fn [key] (get-frequency-count groups key)) group_keys) # => ({:key :spade, :count 3} {:key :heart, :count 1})
    "
    (fn [xs]
      (->> xs
          flatten
          frequencies
          curried-map))))

(defn group-cards [deck]
  """
  (def deck [[:spade :A] [:heart :10] [:spade :9] [:spade :2]])
  (group-cards deck)
  {:spade [:spade :spade :spade], :A [:A], :heart [:heart], :10 [:10], :9 [:9], :2 [:2]}
  """
  (group-by (fn [x] x) (flatten deck)))

(defn get-keys-with-groups [groups]
  """
  groups = Output from group-cards
  (get-keys-with-groups groups)
  [(:spade :A :heart :10 :9 :2) {:spade [:spade :spade :spade], :A [:A], :heart [:heart], :10 [:10], :9 [:9], :2 [:2]}]
  """
  [(keys groups) groups])
  
(defn get-frequency-count [groups key]
  {:key key :count (count (get groups key))})

(defn get-frequencies [[keys groups]]
  """
  Takes the output from get-keys-with-groups as input.
  Output:
  # => ({:key :spade, :count 3} {:key :heart, :count 1})
  """
  (map (fn [key] (get-frequency-count groups key)) keys))

(defn count-occurences [deck]
    "
    To facilitate a better output rather than (3 1) which is yielded by count-occurences-dep
    (def partial-deck [[:spade :A] [:heart :10] [:spade :9] [:spade :2]]) 
    The function for 1. is group-cards
    1. (def groups (group-by (fn [x] (get x 0)) partial-deck)) # => {:spade [[:spade :A] [:spade :9] [:spade :2]], :heart [[:heart :10]]}
    Using keys after group-cards to get the list of (:spade :heart :10 ... etc)
    2. (def group_keys (keys groups)) # => (:spade :heart)
    The function for this needs to be curried with closure over groups... Called count-cards
    3. (map (fn [key] (get-frequency-count groups key)) group_keys) # => ({:key :spade, :count 3} {:key :heart, :count 1})

    USAGE:
    (count-occurences [[:spade :A] [:heart :10] [:spade :9] [:spade :2]])
    Outputs (the structure we'll check against proper-deck):
    ({:key :spade, :count 3} {:key :A, :count 1} {:key :heart, :count 1} {:key :10, :count 1} {:key :9, :count 1} {:key :2, :count 1})
    "
      (->> deck
          group-cards
          get-keys-with-groups
          get-frequencies))

(defn compare-sequences [xs ys]
  "
  Where xs is the generated sequence which we compare against a sequence (ys) for
  which we expect the results to be.

  (.indexOf '({:key 'two'} {:key 'one'}) {:key 'one'})
  1
  poker-proto.core=> (.indexOf '({:key 'two'} {:key 'one'}) {:key 'two'})
  0
  poker-proto.core=> (.indexOf '({:key 'two'} {:key 'one'}) {:key 'three'})
  -1
  "
  (let [nested-compare (fn [element] (.indexOf xs element))
        sequences-equivalent (= (.indexOf (map nested-compare ys) -1) -1)]
  (if sequences-equivalent
    true
    false)))

; (def list-one '(
;      {:key :diamond, :count 13}
;      {:key :club, :count 13}))
;  (def list-two '(
;      {:key :heart, :count 13}
;      {:key :spade, :count 13}))
; (not= list-one list-two) # => true
; (def list-three '(
;     {:key :diamond, :count 13}
;     {:key :club, :count 13}))
; (not= list-one list-three) # => false
; (not false) # => true
; (not (not= list-one list-three)) # => true
; (not (not= list-one list-two)) # => false
; AHHH... but if the elements are in a different order in the list...
; then the not= result will be false...


(deftest build-deck-test
  "
  Just want to ensure the 4 sublists are of length 13 to ensure they add up to 52
  if the list is flattened.
  (filter (fn [x] (= (count x) 12)) '('(1 2) '(3 4))) # => ()
  (filter (fn [x] (= (count x) 2)) '('(1 2) '(3 4))) # =>((quote (1 2)) (quote (3 4)))
  "
  (let [deck (pp/build-deck pp/suits pp/values)
        sub-lists-of-length-thirteen (filter (fn [x] (= (count x) 13)) deck)]
    (is (= (count deck) 4))
    ; Typically, testing something like this would be considered an implementation detail,
    ; which is no bueno. However, in this case, since build-deck is a dependency of construct-deck
    ; and it is expecting the deck to be in this format, keeping this test will help eliminate any confusion
    ; if the implementation of build-deck changes.
    (is (= (count sub-lists-of-length-thirteen) 4))))

(deftest construct-deck-test
  (let [
    deck (pp/construct-deck pp/suits pp/values)
    built-deck-occurences (count-occurences deck)
  ]
  (is (= (count deck) 52))
  (is (= (compare-sequences built-deck-occurences proper-deck) true))))

(deftest mix-cards-test
  (let [deck (pp/construct-deck pp/suits pp/values)
        mixed-cards (pp/mix-cards deck)]
    (is (= (count mixed-cards) 52))))

(deftest shuffle-deck-test
  (let [deck (pp/construct-deck pp/suits pp/values)
        shuffled-deck (pp/shuffle deck 100)]
    (is (=  (count shuffled-deck) 52))
    ; TODO: This should be a function which takes in a shuffled-deck
    ; and a deck, and computes a delta between the two in order to ensure
    ; and a given threshold of randomness in the shuffle is maintained.
    ; (is (= shuffled-deck "WHAT"))
    ))

; ********
; THE BUILD UP OF FUNCTIONS FOR DEALING A HAND:
; ********
(deftest add-card-to-hand-test
(let [player {:name "player1", :seat-position 1, :hand [], :folded false, :bet-size 0}
      player-after-add-card {:name "player1", :seat-position 1, :hand [{:suit :spade, :card-value 10}], :folded false, :bet-size 0}]
    (is (= (pp/add-card-to-hand player {:suit :spade :card-value 10 }) player-after-add-card))))


(deftest deal-test
  (let [player {:name "player1", :seat-position 1, :hand [], :folded false, :bet-size 0}
        shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)
        dealt-hand-state (pp/deal shuffled-deck player)
        updated-player-hand (-> dealt-hand-state :updated-player :hand)
        remaining-deck (:remaining-deck dealt-hand-state)]
        (is (= (count remaining-deck) 51))
        (is (= (count updated-player-hand) 1)
        ; (is (= dealt-hand-state "WHAA"))
        )))

(deftest update-player-hand-test
  (let [shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)
        player-one (get (:players pp/player-state) 0)
        result (pp/update-player-hand pp/player-state shuffled-deck player-one)
        remaining-deck-count (count (:remaining-deck result))
        updated-player-state (:players result)
        updated-player-hand (-> updated-player-state (nth 0) :hand)
        ]
  ; Would be good to ensure the hand has the correct types: i.e. [:heart :10], [:spade, :2]
  ; (is (= result "WHAA"))
  (is (= remaining-deck-count 51))
  (is (= (count updated-player-hand) 1))))

(deftest deal-hand-test
  (let [shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)
        deal-single-hand (pp/deal-hand shuffled-deck)
        result (deal-single-hand pp/player-state (get (:players pp/player-state) 0))
        remaining-deck-count (count (:remaining-deck result))
        updated-player-state (:players result)
        updated-player-hand (-> updated-player-state (nth 0) :hand count)
        ]
        ; (is (= updated-player-state "WUHH"))
        (is (= remaining-deck-count 51))
        (is (= updated-player-hand 1))))

(deftest adjust-players-position-for-dealing-test
  (let [
    game-turn-state {:button-position 3 :player-turn 5}
    result (pp/adjust-players-position-for-dealing (:players pp/player-state) (pp/button-position game-turn-state))]
    (is (= result [{:name "name4", :seat-position :4, :hand [], :folded false, :bet-size 0}
                   {:name "name5", :seat-position :5, :hand [], :folded false, :bet-size 0}
                   {:name "name6", :seat-position :6, :hand [], :folded false, :bet-size 0}
                   {:name "name7", :seat-position :7, :hand [], :folded false, :bet-size 0}
                   {:name "name8", :seat-position :8, :hand [], :folded false, :bet-size 0}
                   {:name "name9", :seat-position :9, :hand [], :folded false, :bet-size 0}
                   {:name "name10", :seat-position :10, :hand [], :folded false, :bet-size 0}
                   {:name "name1", :seat-position :1, :hand [], :folded false, :bet-size 0}
                   {:name "name2", :seat-position :2, :hand [], :folded false, :bet-size 0}
                   {:name "name3", :seat-position :3, :hand [], :folded false, :bet-size 0}]))))

(deftest deal-single-round-test
  (let [
    shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)
    game-turn-state {:button-position 3 :player-turn 5}
    dealt-round-result (pp/deal-single-round shuffled-deck (:players pp/player-state) game-turn-state)
    remaining-deck-count (count (:remaining-deck dealt-round-result))
    updated-player-state (:players dealt-round-result)
    valid-deal? (every? (fn [player] (= (count (:hand player)) 1)) updated-player-state)
    last-idx (- (count updated-player-state) 1)
    first-player-seat-pos (:seat-position (nth updated-player-state 0))
    last-player-seat-pos (:seat-position (nth updated-player-state last-idx))
  ]
  (is valid-deal?)
  (is (= remaining-deck-count 42))
  (is (= first-player-seat-pos :4))
  (is (= last-player-seat-pos :3))))
                               
; (deftest wtf
;   (let [shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)]
;   (is (= (reduce (pp/deal-hand shuffled-deck) {} (:players pp/player-state)) "WTF"))))

; (deftest dealt-initial-hands-test
;   (let [shuffled-deck (pp/shuffle (pp/construct-deck pp/suits pp/values) 100)
;         dealt-hands (pp/deal-initial-hands shuffled-deck (:players pp/player-state) 2)]
;     (is (= dealt-hands "WHAT"))))


; Not going to implement this at the moment... But what property of a shuffled-deck would I want to capture... perhaps
; it's the diff between the built-deck and the distance from which a particular card started and where it moved, and then
; measure those diffs and aggregate them into a final score... and ensure that the score consistently remains above a certain
; threshold so we can guarantee a solid consistent level of randomness in the shuffled deck.

; NOTE:
; You can also organize tests into contexts w/ "testing".
; (deftest test-basic-inventory (testing "Finding books"
;                                 (is (not (nil? (i/find-by-title "Emma" books))))
;                                 (is (nil? (i/find-by-title "XYZZY" books)))) (testing "Copies in inventory"
;                                                                                (is (= 10 (i/number-of-copies-of "Emma" books)))))sdf