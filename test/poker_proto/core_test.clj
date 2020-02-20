(ns poker-proto.core-test
  (:require [clojure.test :refer :all]
            [poker-proto.core :as pp]))

(def proper-deck (
  ; suits
  {:key :spade, :count 13} 
  {:key :heart, :count 13}
  {:key :diamond, :count 13}
  {:key :clubs, :count 13}
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
; (def card-values [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])

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

  (def groups (group-by (fn [x] [(get x 0) (get x 1)] partial-deck))

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
    # NOTE: This group-by approach clearly fails to get the card-values.... Needed to add more
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

(def check-deck-properties (count-occurences (concat pp/suits pp/card-values)))

(deftest build-deck-test
  (is (= (pp/build-deck pp/suits pp/card-values) "WHAAT")))
  ; (is (+ 1 1) (+ 1 2)))
  ; (is (build-deck suits card-values) [:one]))

; NOTE:
; You can also organize tests into contexts w/ "testing".
; (deftest test-basic-inventory (testing "Finding books"
;                                 (is (not (nil? (i/find-by-title "Emma" books))))
;                                 (is (nil? (i/find-by-title "XYZZY" books)))) (testing "Copies in inventory"
;                                                                                (is (= 10 (i/number-of-copies-of "Emma" books)))))