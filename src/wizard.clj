(ns wizard
  (:require [clojure.spec.alpha :as s]))

; so to get types to work i have to understand wtf a type is
; so there's a few possible angles here
; a type is:
;   -- primitive types
;   - some builtin concept, i.e. a primitive type, e.g. an int
;   - a type with a single constructor used for creating trivially isomorphic types
;
;   -- algebraic types
;   - a sum type, i.e. a choice between 2 (or more) types
;   - a product type, i.e. a tuple of types
;
;   -- dependent/first-class types
;   - a dependent function type, i.e. a family of types that vary according to some value,
;     essentially a cartesian product of types. This means we can define, for example,
;     *vectors*, which are lists with a length specified in their type 0-vectors,
;     1-vectors, and 2-vectors are distinct types and we should not expect to be
;     able to necessarily compare them!
;   - a dependent sum type, which corresponds to an existential quantifier; this means
;     that we can define types where the type of the second element of a pair is
;     dependent upon the type of the first element of a pair
;
; so a few builtin types would be good;
;   - Int
;   - String
;   - Bool
;   - Unit (the type with only one value)
;   - Void (the type with no values)


(defrecord Type [name terms type constructors])
(defrecord Term [name multiplicity type])
(defrecord Constructor [name arguments type])
(defrecord FunctionType [arguments return-type])
(defrecord Value [type data])

(defn reference-type [name env]
  (let [type-name (keyword "type" (str name))]
    (prn type-name)
    (prn (keys (:types env)))
    (if (contains? (:types env) type-name)
      type-name
      (throw (Exception. (str "No type known named " type-name))))))

(defn parse-data-type [spec env]
  (if (= 'where (first (second spec)))
    "Complex type"
    (reference-type 'Type env)))

(defn term-if-exists [term typedef]
  (let [term' (get-in typedef [:terms (keyword term)])]
    (if (some? term')
      (keyword term)
      (throw (Exception. (str "Unexpected term " term " in constructor of type " (:name typedef) ". Valid terms: " (:terms typedef)))))))

(defn is-term [term]
  (every? #(Character/isLowerCase %) (str term)))

(defn is-own-type [term typedef]
  (= (str term) (:name typedef)))

(defn is-known-type [term env]
  (contains? (:types env) (keyword "type" (str term))))

(defn parse-constructor-term [term typedef env]
  (cond
    (seq? term) (map #(parse-constructor-term % typedef env) term)
    (is-term term) (term-if-exists term typedef)
    (is-own-type term typedef) (keyword "type" (:name typedef))
    (is-known-type term env) (keyword "type" (str term))
    :else (throw (Exception. (str "No matching term or type for constructor argument: " term)))))

(defn list-terms [typedef]
  (let [terms (vals (:terms typedef))]
    (map #(symbol (:name %)) terms)))

(defn typedef-to-basic-type [typedef]
  (conj (list-terms typedef) (:name typedef)))

;; (defn parse-constructor-arg [arg typedef]
;;   (cond
;;     (is-term arg) (arg (:terms typedef))
;;     (is-type arg) ))

(defn constructor-type [constructor-args typedef]
  (FunctionType. constructor-args (keyword "type" (:name typedef))))

(defn parse-constructor [spec typedef env]
  (if (seq? spec)
    (let [name (first spec)
          terms (rest spec)
          constructor-terms (map #(parse-constructor-term % typedef env) terms)]
      (Constructor. name constructor-terms (constructor-type constructor-terms typedef)))
    (Constructor. spec nil (:name typedef))))

(defn parse-sum-constructors [spec typedef env]
  (map #(parse-constructor %1 typedef env) spec))

(defn parse-data-constructors [spec typedef env]
    (parse-sum-constructors spec typedef env))

(defn parse-data-name [name-and-terms]
  (if (seq? name-and-terms)
    (first name-and-terms)
    name-and-terms))

(defn parse-mult [mult]
  (cond
    (= mult 0) :mult/Erase
    (= mult 1) :mult/Linear
    (= mult 'm) :mult/Unrestricted
    :else nil))

(defn parse-data-term [term env]
  (if (seq? term)
    (let [op (first term)]
      (cond
        (= op '=) (Term. (str (second term)) :mult/Unrestricted (reference-type (nth term 3) env))
        (= op '==) (Term. (str (nth term 3)) (parse-mult (second term)) (reference-type (nth term 4) env))
        :else (throw (Exception. "Invalid term specifier"))))
    (Term. (str term) :mult/Erase :type/Unbound)))

(defn parse-data-terms [name-and-terms env]
  (if (seq? name-and-terms)
    (reduce #(assoc %1 (keyword (:name %2)) %2)
            {}
            (map parse-data-term (rest name-and-terms) env))
    nil))

(defn parse-data [form env]
  (let [data (rest form)
        name-and-terms (first data)
        name (parse-data-name name-and-terms)
        terms (parse-data-terms name-and-terms env)
        spec (rest data)
        type' (parse-data-type spec env)
        constructors (parse-data-constructors (first spec) (Type. (str name) terms type' nil) env)]
    (Type. (str name) terms type' constructors)))

(defn assertion-name [form]
  (let [name (str (second form))]
    (cond
      (is-term name) (keyword name)
      :else (keyword "type" name))))

(defn parse-function-type [signature]
  "not implemented")

(defn parse-type [signature]
  "also not implemented")

(defn assertion-type [form env]
  (let [signature (last form)]
    (cond
      (seq? signature) (cond
                         (= '-> (first signature)) (parse-function-type signature)
                         :else (parse-type signature))
      :else (reference-type signature env))))

(defn typeof-assertion [name env]
  (if (contains? (:assertions env) name)
    (name (:assertions env))
    (throw (Exception. (str "No type assertion for " name)))))

(defn typecheck-def [form env]
  (let [name (assertion-name form)
        value (last form)
        assertion (typeof-assertion name env)
        value-type (:type value)]
    (if (= assertion value-type)
      env
      (throw (Exception. (str "Type mistmatch: " name " should be a " assertion " but is a " value-type))))))

(defn add-constructor [env constructor]
  ; reduce over the constructors
  env
  )

(defn add-constructors [env t]
  env)

(defn typecheck-form [env form]
  (cond
    (= 'data (first form)) (let [t (parse-data form env)]
                             (if (contains? (:types env) (:name t))
                               (throw (Exception. (str "Type " (:name t) " already exists.")))
                               nil)
                             (let [env-with-type (assoc-in env [:types (keyword "type" (:name t))] t)
                                   env-with-type-and-constructors (add-constructors env t)]
                               env-with-type-and-constructors))
    (= 'type (first form)) (assoc-in env [:assertions (assertion-name form)] (assertion-type form env))
    (= 'def (first form)) (typecheck-def form env)
    :else env))

(defn typecheck [env program]
  (reduce typecheck-form env program))

; So I revised the syntax from the ML-style to something lispier
;
; Quick guide:
;   - Int, a, etc.: types and terms
;   - (-> a b): a function type from a to b
;   - (= x a): named term with explicit type
;   - (== 1 x a): named term with explicit multiplicity
;; (def wizard-types-example
;;   ; 'data' introduces a new type into the program
;;   '((data MyInt Int) ; a type alias
;;     (data Pair [(Pair Int Int)]) ; a product type
;;     (data Choice [Int String]) ; a sum type
;;     (data Bool [True False]) ; a bool (sum type with two nullary constructors)
;;     (data (List a) [Nil (Cons a (List a))]) ; a simple homogeneous list
;;     (data IntList [(List Int)] ; a list of Ints
;;     (data OtherList (-> (= a Type) Type) ; Same list, different syntax
;;           (where [Nil (List a)
;;                   "::" (-> a (List a) (List a))]))
;;     (data (Tree a) (+ (Leaf a) (Node (Tree a) (Tree a)))) ; a simple homogeneous tree
;;     (data Nat [Zero
;;                (S Nat)]) ; a natural number, peano-style
;;     (data OtherNat Type ; a natural number using the longer syntax
;;           [(Zero Nat)
;;            (S (-> Nat Nat))])
;;     (data Vect (-> (= n Nat) a Type) ; a more complex dependent type signature
;;           [(Nil (Vect 0 a))
;;            ("::" (-> (= x a) (= xs (Vect n a)) (Vect (S n) a)))])

;;     ; 'type' asserts that a term with a certain name has a certain type
;;     (type a-number Int) ; I have a constant named 'a-number' which is an Int
;;     (type inc (-> Int Int)) ; the type signature of a function that increments an int
;;     (type curried (-> Int Int Int)) ; the type signature of a function with more than one arg
;;     )))

(def initial-env
  {:types {:type/Type (Type. "Type" nil :type/Type nil)
           :type/Int (Type. "Int" nil :type/Type nil)
           :type/String (Type. "String" nil :type/Type nil)}
   :assertions {}
   :type-constructors {}})

(def my-little-program '(
    (data (Maybe a) [(Just a)
                     Nothing])
    (data Point [(Point Int Int)])
    (data (List a) [Nil
                    (Cons a (List a))])

    (type Point Type)

    (type x Int)
    (def x 0)

    (type y String)
    (def y "Hello")

    (type z Point)
    (def z (Point 1 2))

    (type a (Maybe Int))
    (def a (Just 1))
))


(defn parse-primitive [form]
  (cond (seq? form) (map parse-primitive form)
        (string? form) (Value. :type/String form)
        (int? form) (Value. :type/Int form)
        :else form))

(defn parse-primitives [program]
  (map parse-primitive program))

;(parse-primitives '((def x 1)))
;(typecheck initial-env my-little-program)

;; (->> my-little-program
;;      parse-primitives
;;      (typecheck initial-env))

(defrecord DType [name args])

(defn is-dtype? [t]
  (instance? DType t))

(s/def :wizard/dtype is-dtype?)

(defn simple-type [t]
  {:pre [(s/valid? :wizard/dtype t)]}
  (fn [] t))

(defn make-type [t & args]
  (->DType t (vec args)))

(defn make-list-of-a [a]
  (make-type 'List a))

(defn make-pair [a b]
  (make-type 'Pair a b))

(def Int (make-type 'Int))
(def List (->DType 'List ['a])) ; a -> List a
(def Point (->DType 'Point ['Int 'Int])) ; Point Int Int

((simple-type Int))
((simple-type Point))
(make-list-of-a Int)

(def MyList (make-type 'MyList 'a))
(def MyTriple (make-type 'MyTriple 'a 'b 'c))
(def MixedTriple (make-type 'MixedTriple 'a 'Int 'b))
(def Pair (make-type 'Pair 'a 'a))

(defn looks-like-var [var]
  (let [var-name (str var)
        match (re-matches #"[a-z]+" var-name)]
    (some? match)))

(defn get-free-vars [t]
  (let [uniq-args (distinct (:args t))]
    (filter looks-like-var uniq-args)))

(get-free-vars MixedTriple)
;; => (a b)
;;
(defn replace-k [var-name match-name value]
  (if (= var-name match-name)
    value
    var-name))

(defn eval-var [t var-name value]
  (let [args (:args t)
        args' (map #(replace-k % var-name value) args)]
    (apply make-type (list (:name t)) args')))

(eval-var (eval-var MixedTriple 'a 'Int) 'b 'Int)
(eval-var Pair 'a 'Int)
