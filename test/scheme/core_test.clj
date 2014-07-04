(ns scheme.core-test
  (:require [clojure.test :refer :all]
            [scheme.core :refer :all]))

(deftest parsing
  (is (= (parse "1") 1))
  (is (= (parse "(1)") [1]))
  (is (= (parse "(first (1 2))") ["first" [1 2]]))
  (is (= (parse "(first (1 (2)))") ["first" [1 [2]]])))

(deftest eval-built-ins
  (is (= 1 (sch-eval "1")))
  (is (= 6 (sch-eval "(* 2 3)")))
  (is (= 5 (sch-eval "(+ 2 3)")))
  (is (= 8 (sch-eval "(+ 2 (* 2 3))")))
  (is (= '(1 2) (sch-eval "(quote (1 2))")))
  (is (= 1 (sch-eval "(first (quote (1 2)))")))
  (is (= (seq [2 3]) (sch-eval "(rest (quote (1 2 3)))"))))
