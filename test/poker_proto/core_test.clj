(ns poker-proto.core-test
  (:require [clojure.test :refer :all]
            [poker-proto.core :refer :all]))

(def suits [:spade :suit :heart :club :diamond])
(def card-values [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])


(deftest build-deck-test
  (is (+ 1 1) (+ 1 2)))
  ; (is (build-deck suits card-values) [:one]))