(ns poker-proto.core-test
  (:require [clojure.test :refer :all]
            [poker-proto.core :as pp]))

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

(defn count-occurences [terms xs]
  (let [curried-map (map-curried-terms-over-frequencies terms)]
    "
    NOTE:
    if I place (map-curried-terms-over-frequencies terms)
    where curried-map is, I get an error, but with let and curried-map
    assigned to that value, it's no issue.

    Example:
    (count-occurences [:spade :heart] [[:spade :A] [:heart :10] [:spade :9] [:spade :2]])
    (3 1)
    "
    (->> xs
         flatten
         frequencies
         curried-map)))

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