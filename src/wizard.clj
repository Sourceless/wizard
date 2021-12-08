(ns wizard
  (:require [clojure.string :as str]))

(def prelude
  '(
    (data (State s a) [(MkState s a)])
    (def (IO a) (-> W (a World)))
    (type print (-> String (IO World ())))
    (def print [s]
      (MkIO World ()))
    ))

(def hello-world
  (concat prelude
          '(
            (type main (IO ()))
            (def main
              (print "Hello world!"))
            )))

(defn is-lowercase? [str]
  (= str (str/lower-case str)))

(defn type-variables [typespec]
  (let [args (rest typespec)
        vars (filter is-lowercase? (map str args))
        vars (dedupe vars)]
    (map symbol vars)))

(defn add-type [t env]
  (assoc env :types t))

(defn add-type-constructor [t tc env]
  (assoc-in env [:type-constructors t] tc))

(defn curry [func-type]
  (let [args (drop-last 1 (rest func-type))
        return-type (last func-type)]
    (reduce #(list '-> %2 %1) return-type (reverse args))))

(defn type-constructor-from-signature [sig]
  (let [vars (type-variables sig)
        func-args (conj vars '->)
        uncurried (concat (list '->) vars (list sig))]
    (curry uncurried)))

(defn parse-data [typespec env]
  (let [type-and-params (first typespec)]
    (->> env
      (add-type type-and-params)
      (add-type-constructor type-and-params (type-constructor-from-signature type-and-params)))))

(defn parse-assert [assertion env]
  (let [definition-name (first assertion)
        definition-type (rest assertion)]
    (assoc-in env [:assertions definition-name] definition-type)))

(defn parse-definition [definition env]
  (let [definition-name (first definition)
        definition-body (rest definition)]
    (assoc-in env [:definitions definition-name] definition-body)))

(defn parse [env form]
  (cond
    (= 'data (first form)) (parse-data (rest form) env)
    (= 'type (first form)) (parse-assert (rest form) env)
    (= 'def (first form)) (parse-definition (rest form) env)
    :else env))

(defn make-constructor [[k v]]
  ; TODO create constructors for all the primitive types
  [k v])

(defn make-constructors [env]
  (assoc env :constructors
         (map make-constructor (:types env))))

(defn read [program env]
  (-> env
      (#(reduce parse % program))))

(read prelude {:types (hash-set)
               :assertions {}
               :definitions {}
               :type-constructors {}
               :value-constructors {}})
