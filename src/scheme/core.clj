(ns scheme.core
  (:require [clojure.string :as str]))

;; lexing and parsing

(defn tokenize
  [input]
  (-> input
      (str/replace #"\(|\)" #(str " " %1 " "))
      str/trim
      (str/split #" +")))

(defn- parse-atom
  [tok]
  (let [repr (read-string tok)]
    (if (number? repr)
      repr
      tok)))

(defn- parse-list
  [tokens]
  (let [tokens (subvec tokens 1 (- (count tokens) 1))]
    (loop [i 0
           parsed []]
      (if (= i (count tokens))
        parsed
        (if (= (tokens i) "(")
          (let [sublist-end (inc (.lastIndexOf tokens ")"))]
            (recur sublist-end (conj parsed
                                     (parse-list (subvec tokens i sublist-end)))))
          (recur (inc i) (conj parsed (parse-atom (tokens i)))))))))

(defn parse
  [input]
  (let [tokens (tokenize input)]
    (if (= (first tokens) "(")
      (parse-list tokens)
      (parse-atom (first tokens)))))

;; eval

(declare eval-parsed)

(defn- eval-list
  [lst]
  (if (= (first lst) "quote")
    (apply list (second lst))
    ;; needs envs for defining functions and symbols for vars
    (let [func @(resolve (symbol (first lst)))]
      (apply func (map eval-parsed (rest lst))))))

(defn- eval-parsed
  [ast]
  (if (vector? ast)
    (eval-list ast)
    ;; atoms evaluate to themselves
    ast))

(defn sch-eval
  [program]
  (eval-parsed (parse program)))
