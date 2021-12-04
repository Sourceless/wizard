# wizard
A programming language with first-class types, based on cubical and
quantitative type theories.

# Planned Features
* Purity & Referential Transparency (no side-effects)
* Dependent Types (first class types)
* Quantitative Types (explicit type erasure and linearity for resource
  management and optimisation)
* Higher Inductive Types and Univalence Axiom (ability to specify equalities
  above and beyond identity)
* Compiled to LLVM IR, and possibly to WASM too
* No/minimal runtime
* Decent standard library (a decent prelude, some useful http/etc stuff)

# Aims
Create a small language with a strong type theory that can be built upon and
used as a target for higher level languages.

More than anything, I want to bring dependent types to more people, and improve
our programs just a little bit.

# Inspiration
Wizard owes its design and principles to many other projects; here are just a
few.

## Idris
Idris, and Idris 2 specifically, is the first real pacman-complete usage of
dependent types that I have seen.

However, its adherence to the ML-style syntax is, I think, a downside, which
makes it less approachable to those not already in the Haskell-sphere.

## Rust
Rust has fantastic memory safety and lifecycle management, which is something
that I would like Wizard to emulate. In addition, Rust's packaging story is
possibly the best out there at the time of writing (2021).

However, Rust could have gone further with its type system and focuses mainly
on system programming as a primary usecase. This is great, but I want something
with a better type system and a bit more high-level.

## Clojure
Clojure's immutability and lisp syntax makes it the top build-your-own-language
out there today, other than perhaps Racket.

I think, however, it is suffering the same fate as many dynamically typed
languages, where you can only gain so much confidence in the implementation
from static analysis and runtime checking.

## Python
Python's batteries-included standard libraries and approachable nature make it
a default choice for me on some projects. Its strong insistence to stick to
its principles have allowed it to stand the test of time and become something
of a lingua franca.

However, it falls foul of many of the same issues as clojure, but it's also
objectively a less powerful language due to its lack of macros and some of the
early design choices which still plague the runtime. It's also not great to
distribute.
