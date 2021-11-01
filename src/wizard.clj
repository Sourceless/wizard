(ns wizard)

(def wizard-example
  '((type inc (int -> int))
    (defn inc
      ([n]
       (+ n 1)))))


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
(defrecord Constructor [name terms type])

(defn reference-type [name env]
  (let [type-name (keyword "type" (str name))]
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

(defn parse-constructor-term [term typedef env]
  (cond
    (every? #(Character/isLowerCase %) (str term)) (term-if-exists term typedef)
    :else (throw (Exception. (str "Unknown constructor term encountered: " term)))))

(defn list-terms [typedef]
  (let [terms (vals (:terms typedef))]
    (prn terms)
    (map #(symbol (:name %)) terms)))

(defn typedef-to-basic-type [typedef]
  (prn typedef)
  (prn (list-terms typedef))
  (conj (list-terms typedef) (symbol (:name typedef))))

(defn constructor-type [constructor-terms typedef]
  "To be implemented")

(defn parse-sum-constructor [spec typedef env]
  (if (seq? spec)
    (let [name (first spec)
          terms (rest spec)
          constructor-terms (map #(parse-constructor-term % typedef env) terms)]
      (Constructor. name constructor-terms (constructor-type constructor-terms typedef)))
    (Constructor. spec nil (typedef-to-basic-type typedef))))

(defn parse-sum-constructors [spec typedef env]
  (prn spec)
  (map #(parse-sum-constructor %1 typedef env) spec))

(defn parse-data-constructors [spec typedef env]
  (cond
    (= (first spec) '+) (parse-sum-constructors (rest spec) typedef env)
    :else (throw (Exception. "Not implemented"))))

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

(defn typecheck-form [env form]
  (if (= 'data (first form))
    (let [t (parse-data form env)]
      (if (contains? (:types env) (:name t))
        (throw (Exception. (str "Type " (:name t) " already exists.")))
        nil)
      (assoc-in env [:types (:name t)] t))
    env))

(defn typecheck [env program]
  (reduce typecheck-form env program))

; So I revised the syntax from the ML-style to something lispier
;
; Quick guide:
;   - Int, a, etc.: 'simple' types or terms
;   - (= x a): named term with explicit type
;   - (== 1 x a): named term with explicit multiplicity
;   - (+ a a): sum type
;   - (* a a): product type
(def wizard-types-example
  ; 'data' introduces a new type into the program
  '((data MyInt Int) ; a type alias
    (data Pair (* Int Int)) ; a product type
    (data Choice (+ Int String)) ; a sum type
    (data Bool (+ True False)) ; a bool (sum type with two nullary constructors)
    (data IntList [Int]) ; a list of Ints
    (data (List a) (+ Nil (Cons a (List a)))) ; a simple homogeneous list
    (data OtherList (-> (= a Type) Type) ; Same list, different syntax
          (where [Nil (List a)
                  "::" (-> a (List a) (List a))]))
    (data (Tree a) (+ (Leaf a) (Node (Tree a) (Tree a)))) ; a simple homogeneous tree
    (data Nat (+ Zero (S Nat))) ; a natural number, peano-style
    (data OtherNat Type ; a natural number using the longer syntax
          (where [Zero Nat
                  S (-> Nat Nat)]))
    (data Vect (-> (= n Nat) a Type) ; a more complex dependent type signature
          (where [Nil (Vect 0 a)
                  "::" (-> (= x a) (= xs (Vect n a)) (Vect (S n) a))]))

    ; 'type' asserts that a term with a certain name has a certain type
    (type a-number Int) ; I have a constant named 'a-number' which is an Int
    (type inc (-> Int Int)) ; the type signature of a function that increments an int
    (type curried (-> Int Int Int)) ; the type signature of a function with more than one arg
    ))

(def initial-env
  {:types {:type/Type (Type. "Type" nil :type/Type nil)}})

; (typecheck {:types {}} wizard-types-example)

(typecheck initial-env '((data (Maybe a) (+ (Just a) Nothing))))

;; (typecheck {:types {}} '((data MyInt Int)
;;                          (data Pair (Int * Int))))
