(ns wizard)

(def prelude
  '(
    (data IO a)
    (type print (-> String (IO ())))
    (def print [s]
      (print! s))
    ))

(def hello-world
  (concat prelude
          '(
            (type main (IO ()))
            (def main
              (print "Hello world!"))
            )))

(defn parse-data [typespec env]
  (let [type-name (first typespec)
        type-def (rest typespec)]
    (assoc-in env [:types type-name] type-def)))

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
      (#(reduce parse % program))
      make-constructors))

(read prelude {:types {}
               :assertions {}
               :definitions {}
               :constructors {}})
