(ns wizard
  (:require [clojure.walk :refer [prewalk postwalk]]))

(def my-program
  '(((fn x x) (fn _ 0)) 1))

(def my-boring-program
  '(1))

(def program-with-binding
  '(
    (def x 1)
    x
    ))

(def program-with-two-bindings
  '(
    (def x 1)
    (def y 2)
    x
    ))

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

(defn wizard-macro-binding [binding body]
  (println "Expanding" binding)
  (let [name (second binding)
        value (last binding)]
    (list (list 'fn name body) value)))

(defn wizard-macro-expand-form [form]
  (if (and (seq? form) (seq? (first form)) (= 'def (first (first form))))
    (wizard-macro-binding (first form) (second form))
    form))

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

(defn value? [program]
  (number? program))

(defn reducible? [program]
  (seq? program))

(defn terminal? [program]
  (not (reducible? program)))

(defn wizard-macro-expand [program]
  (println program)
  (if (reducible? program)
    (map wizard-macro-expand (wizard-macro-expand-form program))
    program))

(defn function? [program]
  (and (reducible? program) (= 'fn (first program))))

(defn indent [n]
  (apply str (repeat n "  ")))

(defn normal-order-eval [program nesting]
  (println (indent nesting) "EVAL:" program)
  (cond
    (terminal? program) program ; if there is nothing to do, do nothing
    (function? program) program ; functions are values and there is nothing to do here
    (reducible? program) (let    ; we have something we can reduce, so let's reduce it
                             [left (first program)
                              right (second program)]
                           (cond
                             (value? left) left
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

(defn wizard-interpret [program]
  (-> program
      wizard-macro-expand
      wizard-read
      wizard-eval))

(assert (= (wizard-interpret my-program) 0))
(assert (= (wizard-interpret my-boring-program) 1))
(assert (= (wizard-macro-expand program-with-binding) '((fn x x) 1)))
(assert (= (wizard-interpret program-with-binding) 1))

(wizard-macro-expand program-with-binding)

(wizard-macro-expand program-with-two-bindings)
