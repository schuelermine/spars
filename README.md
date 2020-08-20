# `spars`

Spars is a lightweight parsing package that uses sets to store parse results to avoid duplicates.

The name is a contraction of **s**et **pars**e and a pun on the word *sparse*.

---

Spars explicitly does not implement instances of `Monad`, `Applicative` and other classes. This is because using sets requires that all functions need the result type of the parser to be orderable.

Because there are no typeclass instances, the `Parser` type is not a newtype construction, but just a type synonym.

Note: This means that *you cannot use do-notation with Spars*.

---

Spars has a dependency on `containers` in order to use maps and sets from that package.

---

Some of the functions in this package have been written with the help of friendly strangers online.