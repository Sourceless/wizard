(ns wizard
  (:require [clojure.walk :refer [postwalk]]))

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
    (list 'lambda new-name (fmap (replace-if-matches arg-name new-name) body))))

(defn wizard-read-form [form context]
  (if (seq? form)
    (cond
      (= 'lambda (first form)) [(wizard-fn-alpha-conversion (second form) (last form)) context]
      (= 'def (first form)) [form (assoc-in context [:definitions (keyword (second form))] (last form))]
      :else [form context])
    [form context]))

(defn wizard-fn-beta-reduce [f arg]
  (if (and (seq? f) (= 'lambda (first f)))
    (let [arg-name (second f)
          body (last f)]
      (postwalk (replace-if-matches arg-name arg) body))
    (throw (Exception. (str f " is not a function and cannot be applied to " arg)))))

(defn wizard-eval-form [form]
  (cond
    (seq? form) (wizard-fn-beta-reduce (first form) (rest form))
    :else form))

; https://clojuredocs.org/clojure.core/merge
(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn wizard-read
  ([program]
   (wizard-read program {}))
  ([program context]
   (cond
     (seq? program) (let [read-forms (map #(wizard-read % context) program)
                          programs (map first read-forms)
                          contexts (map second read-forms)
                          new-context (apply deep-merge contexts)]
                      (wizard-read-form programs new-context))
     :else (wizard-read-form program context))))

(defn reducible? [program]
  (and
   (seq? program)
   (not (string? program))))

(defn terminal? [program]
  (not (reducible? program)))

(defn function? [program]
  (and (reducible? program) (= 'lambda (first program))))

(defn indent [n]
  (apply str (repeat n "  ")))

(defn lookup [terminal context]
  (let [definitions (:definitions context)
        name (keyword terminal)]
    (if (contains? definitions name)
      (get definitions name)
      (throw (Exception. (str "Unknown binding " terminal))))))

(defn literal? [program]
  (or
   (number? program)
   (string? program)))

(defn normal-order-eval [program context nesting]
  (println (indent nesting) "EVAL:" program)
  (cond
    (literal? program) program ; if it's just a value, return the value
    (terminal? program) (normal-order-eval (lookup program context) context nesting) ; replace the value from lookup and re-evaluate
    (function? program) program ; functions are values and there is nothing to do here
    (reducible? program) (let    ; we have something we can reduce, so let's reduce it
                             [left (first program)
                              right (second program)]
                           (cond
                             (terminal? left) (normal-order-eval (list (lookup left context) right) context (inc nesting))
                             (function? left) (normal-order-eval (wizard-fn-beta-reduce left right) context (inc nesting)) ; apply function
                             (reducible? left) (normal-order-eval (list (normal-order-eval left context (inc nesting)) right) context nesting) ; the left side can be reduced

                             ; TODO: these should error
                             (literal? left) program ; nothing we can do, since functions don't have names yet
                             :else program))
    :else program
    )
  )

(defn wizard-eval [[program context]]
  (normal-order-eval program context 0))

(defn wizard-load-main [[program context]]
  (if (contains? (:definitions context) :main)
    [(get-in context [:definitions :main]) context]
    (throw (Exception. "Expected to find definition for main, but no definition of main found."))))

(defn wizard-interpret [program]
  (-> program
      wizard-read
      wizard-load-main
      wizard-eval))

(def prelude
  '((def identity (lambda x x))
    (def true (lambda x (lambda y x)))
    (def false (lambda x (lambda y y)))
    (def cond (lambda x (lambda y (lambda c ((c x) y)))))
    (def and (lambda x (lambda y (((cond y) false) x))))
    (def or (lambda x (lambda y (((cond y) true) x))))))

(def my-program
  '(def main (((lambda x x) (lambda _ 0)) 1)))
(assert (= (wizard-interpret my-program) 0))

(def my-boring-program
  '(def main 1))
(assert (= (wizard-interpret my-boring-program) 1))

(def my-function-call
  '((def identity (lambda x x))
    (def main (identity 1))))
(assert (= (wizard-interpret my-function-call) 1))

(def my-function-call-string
  '((def identity (lambda x x))
    (def main (identity "hello"))))
(assert (= (wizard-interpret my-function-call-string) "hello"))

(def logic-true
  (concat prelude
          '((def main ((true 1) 0)))))
(wizard-interpret logic-true)

(def logic-cond
  (concat prelude
          '((def main cond))))
(wizard-interpret logic-cond)
