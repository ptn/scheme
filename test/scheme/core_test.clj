(ns scheme.core-test
  (:require [clojure.test :refer :all]
            [scheme.core :refer :all]))

(deftest test-parsing
  (is (= (parse "1") 1))
  (is (= (parse "(1)") [1]))
  (is (= (parse "(first (1 2))") ["first" [1 2]]))
  (is (= (parse "(first (1 (2)))") ["first" [1 [2]]])))
