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
      ;; no strings
      tok)))

;; assume balanced parens for now
(defn- matching-paren-pos
  [tokens left-paren-pos]
  (loop [i (inc left-paren-pos)
         nesting 0]
    (cond
     (= (tokens i) ")")
     (if (= nesting 0) i (recur (inc i) (dec nesting)))

     (= (tokens i) "(")
     (recur (inc i) (inc nesting))

     :else
     (recur (inc i) nesting))))

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
          (let [sublist-end (inc (matching-paren-pos tokens parse-here))]
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

;; add let and with for contexts
(def builtins {"+" (fn [scope & args] (apply + args))
               "*" (fn [scope & args] (apply * args))
               "car" (fn [scope x] (first x))
               "cdr" (fn [scope x] (rest x))
               "t" true})

(defn scope-get
  [scope k]
  ;; adding ["f" nil] or ["f" false] to the builtins map confuses
  ;; the ifs below
  (if (= "f" k)
    nil
    (if-let [val (get scope k)]
      val
      (if-let [parent (:parent scope)]
        (recur parent k)
        (throw (Exception. (str "Undefined " k)))))))

(defn- eval-symbol
  [sym scope]
  [(scope-get scope sym) scope])

(defn- sch-lambda
  [params body]
  {:args params :body body})

(defn- sch-lambda? [x] (map? x))

(defn- eval-begin
  [scope forms]
  (reduce (fn [[val scope'] form]
            (eval-parsed form scope'))
          [nil scope]
          forms))

;; function invocation is just normal evaluation of expressions under
;; nested scopes
(defn- invoke-lambda
  [func scope params]
  (let [call-scope (reduce #(assoc %1 (first %2) (second %2))
                           {:parent scope}
                           (map (fn [x y] [x y]) (:args func) params))]
    ;; implicit begin
    (eval-begin call-scope (:body func))))

(defn- eval-list
  [lst scope]
  (condp = (first lst)
    ;; special forms
    "quote" [(apply list (second lst)) scope]
    "define" (let [[val scope'] (eval-parsed (last lst) scope)]
                [val (assoc scope' (second lst) val)])
    "begin" (eval-begin scope (rest lst))
    "lambda" [(sch-lambda (second lst) (subvec lst 2)) scope]
    ;; anything that's not f is true, there's also t as the literal truth value
    "if" (let [[test then else] (rest lst)
               [test-val scope'] (eval-parsed test scope)]
           (eval-parsed (if test-val then else) scope'))
    ;; function invocation
    (let [func (first (eval-parsed (first lst) scope))
          params (map #(first (eval-parsed % scope)) (rest lst))]
      (if (sch-lambda? func)
        (invoke-lambda func scope params)
        [(apply func scope params) scope]))))

(defn- eval-parsed
  [ast scope]
  (cond
   (vector? ast) (eval-list ast scope)
   ;; no string support yet
   (string? ast) (eval-symbol ast scope)
   :else [ast scope]))

(defn sch-eval
  "Return the value of the expression plus the resulting scope of
  doing so."
  ([expr] (sch-eval expr builtins))
  ([expr scope]
     (eval-parsed (parse expr) scope)))

(defn sch
  []
  (println "Type (q) to exit.")
  (loop [scope builtins]
    (print "sch> ")
    (flush)
    (let [input (str/trim (read-line))]
      (when-not (= input "(q)")
        (let [[val scope'] (try
                             (sch-eval input scope)
                             (catch Exception e
                               (do
                                 (println (.getMessage e))
                                 [nil scope])))]
          (when val (println val))
          (recur scope'))))))
