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
  (def STRING  :string      )

(def errors
  {:cparen  "Found unexpected ')'. Maybe you forgot a '('?"
   :unknown "Oops! Something in your code is NOT an implemented token yet. I shall get to it eventually. (sorry for vague error, I'm trying my best!)"
   :eoi     "You ended input unexpectedly... Why'd you leave me? Come back papa please"})

(def default-env
  {"+"      +
   "-"      -
   "*"      *
   "/"      /
   "^"      math/pow
   "vec"    vector
   "hash"   hash-map
   "hget"   get
   "=?"     =
   "!=?"    not=
   "num?"   number?
   "<?"     <
   ">?"     >
   "printn" println
   "true"   true
   "false"  false
   "do"     (fn [& args] (last args))})

(def env (atom default-env))

(defrecord token [type value])

(defn tokenst1 [s]
  (re-seq #"\"[^\"]*\"|\(|\)|[^\s\(\)\"]+" s))

(defn tokenst2 [split]
  (for [tok split]
    (cond
      (= tok "(")                                                                  (->token OPAREN  nil)
      (= tok ")")                                                                  (->token CPAREN  nil)
      (re-matches #"\".*?\"" tok)                                                  (->token STRING  (str/replace tok "\"" ""))
      (re-matches #"[a-zA-Z+\-=\!/\*^<>.,;?][a-zA-Z0-9+\-=\!/\*^<>.,;?]*" tok)     (->token IDENT   tok)
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
      (= (:type tok)  :string)    [tok rest]
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
                  (when (seq (rest (:params f)))
                    (doseq [[k v] (map vector (map :value (rest (:params f))) vals)]
                      (swap! env assoc k v)))
                  (keval (:body f)))
      (fn? f)  (apply f vals)
      :else    (throw (Exception. (str "Hey... '" (:value head) "' isn't a function... If you were expecting some cool Lambda-calculus-style stuff, that's not here."))))))

(def opregexp #"[+\-/*^]")

(declare eval-def)
(declare eval-if)
(declare eval-fn)
(declare eval-op-eq)
(declare eval-loop)
(declare eval-join)
(declare eval-while)
(defn keval [node]
  (cond
    (= (:type node) :integer)    (:value node)
    (= (:type node) :string)     (:value node)
    (= (:type node) :identifier) (let [val (get @env (:value node))]
                                   (when (nil? val)
                                     (throw (Exception. (str "Oops! '" (:value node) "' doesn't exist!"))))
                                   val)
    (vector? node)               (cond
                                   (= (:value (first node)) "=")                     (eval-def (rest node))
                                   (= (:value (first node)) "if")                    (eval-if (rest node))
                                   (= (:value (first node)) "fn")                    (eval-fn (rest node))
                                   (= (:value (first node)) "loop")                  (eval-loop (rest node))
                                   (= (:value (first node)) "join")                  (eval-join (rest node))
                                   (= (:value (first node)) "while")                 (eval-while (rest node))
                                   (re-matches #"=[+\-/*^]" (:value (first node)))   (eval-op-eq node)
                                   :else (apply-fn node))))

(def ops (into {} (filter #(re-matches opregexp (key %)) default-env)))

(defn eval-while [args]
  (let [condition (first args)
        body      (second args)]
    (loop []
      (when (keval condition)
        (keval body)
        (recur)))))

(defn eval-join [args]
  (let [spaces?     (keval (first args))
        theRealArgs (map keval (rest args))
        spaces      (interpose " " theRealArgs)]
    (if (= spaces? true)
      (apply str spaces)
      (apply str theRealArgs))))

(defn eval-loop [args]
  (let [n    (keval (first args))
        body (second args)]
    (loop [i 0]
      (when (< i n)
        (keval body)
        (recur (+ i 1))))))

(defn eval-op-eq [args]
  (let [op    (:value (first args))
        name  (:value (second args))
        value (keval (nth args 2))
        f     (get ops (str/replace op "=" ""))]
    (swap! env assoc name (f (@env name) value))
    (@env name)))

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
  (if (= (str/trim expr) "exit")
    (->result nil 1)
    (if (= (str/trim expr) "clear")
      (do (reset! env default-env)
          (->result nil 0))
      (let [tokens (tokenst2 (tokenst1 expr))]
        (when (= (:type (first tokens)) :cparen)
          (throw (Exception. (:cparen errors))))
        (let [[node _] (parse-expr tokens)]
          (->result (keval node) 0))))))

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