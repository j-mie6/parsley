# Parsley Internals (`parsley.internal.deepembedding`)

> _Here be Dragons!_

Parsley's so-called _combinator tree_ is split into two halves:

* The first half, `frontend.LazyParsley` is a lazy combinator tree that retains the
  laziness characteristics of the public API. This is important, as any recursion in
  a parser must be identified by the library and factored out, to allow for
  traversal: this means that `frontend.LazyParsley` may be an infinite cyclic graph,
  and must remain lazy. The role of this combinator tree is to find and factor out
  shared (which may or may not be recursive) parsers, identify what registers are
  required by a parser, and translate it into the next combinator tree.

* The second half, `backend.StrictParsley` is a strict combinator tree that is a
  finite acyclic tree. This is produced as a produce of the processing of
  `LazyParsley` and is thread- and parser-local. This means it can be mutable and
  take more liberties with the representation of the combinators. This combinator
  tree is used to perform optimisations as well as code generation into the final
  instruction array.

There are some combinator tree nodes that are "leaf" nodes, and as such have no
strictness requirements, lazy or otherwise. These nodes can actually be shared
across both sides of the tree, and are stored in `singletons`.
