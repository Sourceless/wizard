(ns wizard)

(defn -main
  [& _]
  (println "Hello world!"))

(def my-program
  '(
    (fn [x] x)
   ))

(defn wizard-fn [argspec body]
  ; TODO
)

(def initial-env
  {:functions {'+ +
               '- -
               '* *
               '/ /
               'fn wizard-fn}})

(defn is-expression? [form]
  (list? form))

(defn wizard-eval-form [env form]
  (if (is-expression? form)
    (let [function (first form)
          arguments (rest form)]
      (if (contains? (:functions env) function)
        (let [f (get (:functions env) function)]
          (apply f (map #(wizard-eval-form env %) arguments)))
        (throw (Exception. (str "No such function " function)))))
    form))

(defn wizard-eval [env program]
  (wizard-eval-form env (first program)))

(wizard-eval initial-env my-program)
