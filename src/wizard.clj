(ns wizard
  (:require [clojure.walk :refer [postwalk]]))

(defn replace-if-matches [old-name new-name]
  (fn [form]
    (if (= form old-name)
      new-name
      form)))

(defn lambda? [form]
  (and (seq? form) (= 'lambda (first form)) (= (count form) 3)))

(defn macro? [form]
  (and (seq? form) (= 'defmacro (first form)) (= (count form) 3)))

(defn wizard-alpha [program]
  (cond (lambda? program) (let [arg-name (second program)
                             body (last program)
                             new-arg-name (gensym (str arg-name "__"))]
                         (list 'lambda new-arg-name (postwalk (replace-if-matches arg-name new-arg-name) (wizard-alpha body))))
        (seq? program) (map wizard-alpha program)
        :else program))

;; (defn wizard-alpha-macros [program]
;;   (cond (macro? program) (let [arg-name (second program)
;;                              body (last program)
;;                              new-arg-name (gensym (str "macro__" arg-name "__"))]
;;                          (list 'lambda new-arg-name (postwalk (replace-if-matches arg-name new-arg-name) (wizard-alpha-macros body))))
;;         (seq? program) (map wizard-alpha-macros program)
;;         :else program))

(defn wizard-read-form [form context]
  (if (seq? form)
    (cond
      (= 'def (first form)) [form (assoc-in context [:definitions (keyword (second form))] (last form))]
      :else [form context])
    [form context]))

(defn wizard-read-macros-form [form context]
  (if (seq? form)
    (cond
      (= 'defmacro (first form)) [form (assoc-in context [:macros (keyword (second form))] (last form))]
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

(defn wizard-read-macros
  ([program]
   (wizard-read-macros program {}))
  ([program context]
   (cond
     (seq? program) (let [read-forms (map #(wizard-read-macros % context) program)
                          programs (map first read-forms)
                          contexts (map second read-forms)
                          new-context (apply deep-merge contexts)]
                      (wizard-read-macros-form programs new-context))
     :else (wizard-read-macros-form program context))))

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
   ;; (println (indent nesting) "EVAL:" program)
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

(defn wizard-load-main [[_ context]]
  (if (contains? (:definitions context) :main)
    [(get-in context [:definitions :main]) context]
    (throw (Exception. "Expected to find definition for main, but no definition of main found."))))

(defn is-macro [name context]
  (contains? (:macros context) (keyword name)))

(defn replace-macro [name context]
  (get-in context [:macros (keyword name)]))

(defn wizard-macro-expand [[program context]]
  (println [program context])
  (cond
    (terminal? program) (if (is-macro program context)
                          (wizard-macro-expand [(replace-macro program context) context])
                          program)
    (reducible? program) (map #(wizard-macro-expand [% context]) program)
    :else program))

(defn is-macro-defn? [program]
  (and (reducible? program) (= 'defmacro (first program))))

(defn wizard-delete-macro-defns [[program context]]
  [(remove is-macro-defn? program) context])

(defn wizard-interpret [program]
  (-> program
      wizard-alpha ; replace identifiers inside function definitions to prevent accidental capture
      wizard-read-macros ; find out what macro declarations we have
      wizard-delete-macro-defns ; get rid of macro defns, they are no longer needed!
      wizard-macro-expand ; replace macros with their definitions
      wizard-read ; find out what definitions there are
      wizard-load-main ; load the main definition as the entrypoint
      wizard-eval)) ; eval in (lazy) normal-order

(def prelude
  '((def lc-id (lambda x x))
    (def lc-t (lambda x (lambda y x)))
    (def lc-f (lambda x (lambda y y)))
    (def lc-cond (lambda x (lambda y (lambda c ((c x) y)))))
    (def lc-and (lambda x (lambda y (((cond y) false) x))))
    (def lc-or (lambda x (lambda y (((cond y) true) x))))))

; shows that function execution works as expected
(def my-program
  '(def main (((lambda x x) (lambda _ 0)) 1)))
(assert (= (wizard-interpret my-program) 0))

; shows that main is executed
(def my-boring-program
  '(def main 1))
(assert (= (wizard-interpret my-boring-program) 1))

; shows that a defined function can be called
(def my-function-call
  '((def identity (lambda x x))
    (def main (identity 1))))
(assert (= (wizard-interpret my-function-call) 1))

; shows that strings can be returned
(def my-function-call-string
  '((def identity (lambda x x))
    (def main (identity "hello"))))
(assert (= (wizard-interpret my-function-call-string) "hello"))

; lambda calculus true
(def logic-true
  (concat prelude
          '((def main ((lc-t 1) 0)))))
(assert (= (wizard-interpret logic-true) 1))

; Basic test showing that macros do indeed expand
(def macro-test
  '((defmacro x 1)
    (def main x)))
(assert (= (wizard-interpret macro-test) 1))

; This shows that the macro does not pollute the namespaces of functions
(def macro-hygeine-test
  '((defmacro a 1)
    (def identity (lambda a a))
    (def main (identity 2))))
(wizard-interpret macro-hygeine-test)
