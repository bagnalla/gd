# gdtypes
GDScript parser and typechecker.

This is an experimental typechecker for GDScript as it appears in
Godot 3.1. As of now, it's essentially a copy of the existing type
system with a bit more safety (better to have type errors than runtime
errors) and typed containers (arrays and dictionaries are parametric
in the types of their keys/elements). The parser uses megaparsec.

### Issues / TODO:
* 'as' binary operator not supported.
* The unary '.' syntax for accessing properties/methods of the parent
  class doesn't parse correctly.
* 'self' may not (I don't think it does) have constants or static
  functions in its namespace. Shouldn't be a big problem.
* 'setget' annotations on variables aren't checked for existence /
  correct types yet.
* 'class_name' and 'signal' declarations aren't checked.
* Virtual methods not accounted for.
* varargs not supported.
* Need an unsafe cast operation (maybe 'as'?).

Right now some expressions will just default to dynamic if any of the
subexpressions are dynamic. This allows purely dynamic programs to get
through the typechecker (since some things e.g. integer literals will
have static types). But, given the aim of the project, this will
probably change so that some type annotations or casts are actually
necessary to get programs to typecheck.

### Possible extensions
* Parametric polymorphism for user-defined classes and functions.
* Type inference. A straightforward HM style approach via unification
  of type equality constraints won't quite work due to the presence of
  subtyping, but perhaps the following can work. The
  constraints are "is subtype of"
  constraints rather than equality to account for the subsumption
  principle, and unification of incompatible types will yield their
  least upper bound rather than failing with a type
  error. Intersection types could be useful here as well to serve as
  more fine-grained l.u.b.s on types that are completely incompatible
  (e.g., int and String could unify to become (int | String)).
