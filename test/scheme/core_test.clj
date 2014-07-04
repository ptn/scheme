(ns scheme.core-test
  (:require [clojure.test :refer :all]
            [scheme.core :refer :all]))

(deftest parsing
  (is (= (parse "1") 1))
  (is (= (parse "(1)") [1]))
  (is (= (parse "(first (1 2))") ["first" [1 2]]))
  (is (= (parse "(first (1 (2)))") ["first" [1 [2]]]))
  (is (= (parse "(+ (+ 1 2) (* 3 4))") ["+"
                                        ["+" 1 2]
                                        ["*" 3 4]])))

(deftest scopes
  (is (= 3 (scope-get {"x" 3} "x")))
  (is (= 3 (scope-get {"y" 4 :parent {"x" 3}} "x")))
  (try
    (scope-get {"y" 4 :parent {"x" 3}} "z")
    (catch Exception e (is (= "Undefined z" (.getMessage e)))))
  (try
    (sch-eval "(begin (lambda () (define x 2) x) x)")
    (catch Exception e (is (= "Undefined x" (.getMessage e))))))

(deftest eval-forms
  (is (= 1 (sch-eval "1")))
  (is (= 6 (sch-eval "(* 2 3)")))
  (is (= 5 (sch-eval "(+ 2 3)")))
  (is (= 8 (sch-eval "(+ 2 (* 2 3))")))
  (is (= '(1 2) (sch-eval "(quote (1 2))")))
  (is (= 1 (sch-eval "(car (quote (1 2)))")))
  (is (= (seq [2 3]) (sch-eval "(cdr (quote (1 2 3)))")))
  (is (= 4 (sch-eval "(begin (define x 3) (+ x 1))")))
  (is (= 2 (sch-eval "((lambda (x) x) 2)")))
  (is (= 2 (sch-eval "((lambda () 2))")))
  (is (= 3 (sch-eval "((lambda () (define x 2) (+ x 1)))")))
  (is (= 5 (sch-eval "(begin (define y 3) ((lambda (x) (+ x y)) 2))")))
  (is (= 7 (sch-eval "(if 1 (+ 3 4) (* 8 7))")))
  (is (= 10 (sch-eval "(if (+ 1 6) (+ 3 7) (* 8 7))")))
  (is (= 10 (sch-eval "(begin (define x 4) (if (+ x 6) (+ 3 7) (* 8 7)))")))
  (is (= 17 (sch-eval "(if t (+ 3 14) (* 8 7))")))
  (is (= 56 (sch-eval "(if f (+ 3 4) (* 8 7))")))
  (is (= 7 (sch-eval "(if (define x 3) (+ x 4) (* 8 7))"))))
