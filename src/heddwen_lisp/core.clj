(ns heddwen-lisp.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.math   :as math]))

(defrecord result [output break])

;;; Token types
(def   OPAREN  :oparen      )
  (def CPAREN  :cparen      )
  (def IDENT   :identifier  )
  (def UNKNOWN :unknown     )
  (def INT     :integer     )

(def errors
  {:cparen  "Found unexpected ')'. Maybe you forgot a '('?"
   :unknown "Oops! Something in your code is NOT an implemented token yet. I shall get to it eventually. (sorry for vague error, I'm trying my best!)"
   :eoi     "You ended input unexpectedly... Why'd you leave me? Come back papa please"})

(def default-env
  {"+"    +
   "-"    -
   "*"    *
   "/"    /
   "^"    math/pow
   "vec"  vector
   "=?"   =
   "num?" number?})

(def env (atom default-env))

(defrecord token [type value])

(defn tokenst1 [s]
  (remove #(= % "")
    (-> s
      (str/replace "(" " ( ") ; Open   paren
      (str/replace ")" " ) ") ; Closed paren
      (str/split   #"\s+")))) ; ... Split!!!!

(defn tokenst2 [split]
  (for [tok split]
    (cond
      (= tok "(")                                                                  (->token OPAREN  nil)
      (= tok ")")                                                                  (->token CPAREN  nil)
      (re-matches #"[a-zA-Z+\-=\!/\*^<>.,;\"?][a-zA-Z0-9+\-=\!/\*^<>.,;\"?]*" tok) (->token IDENT   tok)
      (re-matches #"[0-9]+\.?[0-9]*" tok)                                          (->token INT     (Double/parseDouble tok))
      :else                                                                        (->token UNKNOWN tok))))

(declare parse-list)
(defn    parse-expr [tokens]
  (when (empty? tokens)
    (throw (Exception. (:eoi errors))))
  (let [[tok & rest] tokens]
    (cond
      (= (:type tok) :oparen)     (parse-list rest)
      (= (:type tok) :integer)    [tok rest]
      (= (:type tok) :identifier) [tok rest]
      (= (:type tok) :unknown)    (throw (Exception. (:unknown errors))))))

(defn parse-list [tokens]
  (loop [remaining tokens
         acc       []]
    (let [[tok & rest] remaining]
      (cond
        (= (:type tok) :cparen) [acc rest]
        :else (let [[expr leftover] (parse-expr remaining)]
                (recur leftover (conj acc expr)))))))

(declare keval)
(defn apply-fn [node]
  (let [[head & args] node
        f    (keval head)
        vals (map #(keval %) args)]
    (cond
      (map? f) (do
                 (doseq [[k v] (map vector (map :value (rest (:params f))) vals)]
                   (swap! env assoc k v))
                 (keval (:body f)))
      (fn? f)  (apply f vals)
      :else    (throw (Exception. (str "Hey... '" (:value head) "' isn't a function... If you were expecting some cool Lambda-calculus-style stuff, that's not here."))))))

(declare eval-def)
(declare eval-if)
(declare eval-fn)
(defn keval [node]
  (cond
    (= (:type node) :integer)    (:value node)
    (= (:type node) :identifier) (let [val (get @env (:value node))]
                                   (when (= (:value node) "exit")
                                     (->result nil 1))
                                   (when (nil? val)
                                     (throw (Exception. (str "Oops! '" (:value node) "' doesn't exist!"))))
                                   val)
    (vector? node)               (cond
                                   (= (:value (first node)) "=")  (eval-def (rest node))
                                   (= (:value (first node)) "if") (eval-if (rest node))
                                   (= (:value (first node)) "fn") (eval-fn (rest node))
                                   :else (apply-fn node))))

(defn eval-fn [args]
  (let [name      (:value (first args))
        arguments (second args)
        body      (nth args 2)]
    (swap! env assoc name
      {:params arguments :body body})
    (@env name)))

(defn eval-if [args]
  (let [condition (keval (first args))
        then      (second args)
        else      (nth args 2 nil)]
    (if condition
      (keval then)
      (when else (keval else)))))

(defn eval-def [args]
  (let [name  (:value (first args))
        value (keval  (second args))]
    (swap! env assoc name value)
    value))

(defn kout [expr]
  (let [tokens (tokenst2 (tokenst1 expr))]
    (when (= (:type (first tokens)) :cparen)
      (throw (Exception. (:cparen errors))))
    (let [[node _] (parse-expr tokens)]
      (->result (keval node) 0))))

(defn -main []
  (loop []
    (print "user=> ")
    (flush)
    (let [should-break
          (try
            (let [input (read-line)
                  res   (kout input)]
              (println (:output res))
              (not= (:break res) 0))
            (catch Exception e
              (println "error:" (.getMessage e))
              false))]
      (when (not should-break)
        (recur)))))