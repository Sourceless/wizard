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

(defn eval-defs [env body]
  (let [name (first body)
        sig (rest body)]
    (assoc-in env [:types name] sig)))

(defn eval-defn [env body])

(def builtins
  {'defs eval-defs
   'defn eval-defn
   '+ +})

(defn eval-form [form env]
  (let [f (get (:functions env) (first form))]
    (f env (rest form))))

; When we eval we should actually do a few things first
;   1. type check
;        iterate over all the type and function definitions in the program
;        and check that they are logically sound. if that's good, then we
;        can eval knowing the program is typesafe
;   2. actually that's it, the second step is to run the eval
;   3. oh actually tbf we can pretty much erase types to some degree after
;        we're sure that the program typechecks, i.e. we don't need to keep
;        the typedefs and signatures around anymore, we just need to have
;        the bits of the program that do stuff
;   4. if we were compiling there would be a bunch more here but since we're
;        interpreting and abusing the shit out of having a host language with
;        a nice library of functions, we can avoid this for now!
(defn eval-wizard [env program]
  ; for a simple repl, this should actually be something like
  ; (-> macroexpand
  ;     typecheck
  ;     eval)
  (reduce eval-form env program))

(defn typecheck-form [form env])

(defn typecheck [env program]
  (reduce typecheck-form env program))

(def initial-env
  {:types {}
   :functions builtins})

; (eval-form '(defs foo (a -> a -> a)) initial-env)
; (eval-wizard wizard-example)

(def wizard-types-example
  ; 'data' introduces a new type into the program
  '((data MyInt Int) ; a type alias
    (data Pair (Int, Int)) ; a product type
    (data Choice (Int | String)) ; a sum type
    (data Bool (True | False)) ; a bool (sum type with two nullary constructors)
    (data List [a] (Nil | Cons a (List a)))
    (data Tree [a] (Leaf a | Node (Tree a) (Tree a))) ; a simple homogeneous tree

    ; 'type' asserts that a term with a certain name has a certain type
    (type a-number Int) ; I have a constant named 'a-number' which is an Int
    (type inc (Int -> Int)) ; the type signature of a function that increments an int
    ))
