(ns wizard
  (:require [clojure.walk :refer [postwalk]]))

(def my-program
  '(((fn x x) (fn _ 0)) 1))

(def my-boring-program
  1)

(defn fmap [f data]
  (if (seq? data)
    (map f data)
    (f data)))

(defn replace-if-matches [old-name new-name]
  (fn [form]
    (if (= form old-name)
      new-name
      form)))

(defn wizard-fn-alpha-conversion [arg-name body]
  ; Replace arg in body with a unique value
  (let [new-name (gensym (str arg-name "__"))]
    (list 'fn new-name (fmap (replace-if-matches arg-name new-name) body))))

(defn wizard-read-form [form]
  (if (seq? form)
    (cond
      (= 'fn (first form)) (wizard-fn-alpha-conversion (second form) (last form))
      :else form)
    form))

(defn wizard-fn-beta-reduce [f arg]
  (if (and (seq? f) (= 'fn (first f)))
    (let [arg-name (second f)
          body (last f)]
      (postwalk (replace-if-matches arg-name arg) body))
    (throw (Exception. (str f " is not a function and cannot be applied to " arg)))))

(defn wizard-eval-form [form]
  (cond
    (seq? form) (wizard-fn-beta-reduce (first form) (rest form))
    :else form))

(defn wizard-read [program]
  (postwalk wizard-read-form program))

(defn reducible? [program]
  (seq? program))

(defn terminal? [program]
  (not (reducible? program)))

(defn function? [program]
  (and (reducible? program) (= 'fn (first program))))

(defn indent [n]
  (apply str (repeat n "  ")))

; TODO debugme
(defn normal-order-eval [program nesting]
  (println (indent nesting) "EVAL:" program)
  (cond
    (terminal? program) program ; if there is nothing to do, do nothing
    (function? program) program ; functions are values and there is nothing to do here
    (reducible? program) (let    ; we have something we can reduce, so let's reduce it
                             [left (first program)
                              right (second program)]
                           (cond
                             (terminal? left) program ; nothing we can do, since functions don't have names yet
                             (function? left) (normal-order-eval (wizard-fn-beta-reduce left right) (inc nesting)) ; apply function
                             (reducible? left) (normal-order-eval (list (normal-order-eval left (inc nesting)) right) nesting) ; the left side can be reduced
                             :else program))
    :else program
    )
  )

(defn wizard-eval [program]
  (println)
  (println "START EVAL")
  (normal-order-eval program 0))

(wizard-eval (wizard-read my-program))

(defn wizard-read-and-eval [program]
  (-> program
      wizard-read
      wizard-eval))

(assert (= (wizard-read-and-eval my-program) 0))
(assert (= (wizard-read-and-eval my-boring-program) 1))
