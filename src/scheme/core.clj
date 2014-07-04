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
      ;; add strings
      tok)))

(defn- parse-list
  [tokens]
  (let [tokens (subvec tokens 1 (- (count tokens) 1))]
    (loop [parse-here 0
           parsed-items []]
      (if (= parse-here (count tokens))
        parsed-items
        (if (= (tokens parse-here) "(")
          ;; parse the entire sublist as one element.  set parse-here
          ;; to past the matching paren, to the beginning of the next
          ;; item.
          (let [sublist-end (inc (.lastIndexOf tokens ")"))]
            (recur sublist-end
                   (conj parsed-items
                         (parse-list (subvec tokens parse-here sublist-end)))))
          (recur (inc parse-here)
                 (conj parsed-items (parse-atom (tokens parse-here)))))))))

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
