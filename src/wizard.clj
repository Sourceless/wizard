(ns wizard)

(defn -main
  [& _]
  (println "Hello world!"))

(def my-program
  '(
    (- 1 2)
    ))


(def initial-env
  {:functions {'+ +
               '- -
               '* *
               '/ /}})

(defn wizard-eval-form [env form]
  (let [function (first form)
        arguments (rest form)]
    (if (contains? (:functions env) function)
      (do
        (println function)
        (println arguments)
        (let [f (get (:functions env) function)]
          (apply f arguments)))
      (throw (Exception. (str "No such function " function))))))

(defn wizard-eval [env program]
  (wizard-eval-form env (first program)))

(wizard-eval initial-env my-program)
