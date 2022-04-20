package parsley

/** This package contains various functionality relating to the parsing of expressions..
  *
  * This includes the "chain" combinators, which tackle the left-recursion problem and
  * allow for the parsing and combining of operators with values. It also includes
  * functionality for constructing larger precedence tables, which may even vary the
  * type of each layer in the table, allowing for strongly-typed expression parsing.
  *
  * @groupprio Chains 0
  * @groupname Chains Chain Combinators
  * @groupdesc Chains
  *     These are all modules that contain the standalone chain combinators. These are useful for parsing the application
  *     of operators to values in many different configurations, including: prefix, postfix, infix (left and right associated),
  *     and mixed fixity. These combinators do have their uses, however they will be overshadowed by the `precedence` combinator,
  *     which allows for the combining of multiple levels of chaining in a clean and concise way.
  *
  * @groupprio Precedence 10
  * @groupname Precedence The Precedence Combinator
  * @groupdesc Precedence
  *     This object contains `apply` methods which represent each of the different "shapes" of precedence combinator, which
  *     takes a description of the precedence relations between various operators and constructs a parser for them.
  *
  * @groupprio Builders 20
  * @groupname Builders Precedence Layer Builders
  * @groupdesc Builders
  *     These are used to construct an individual layer of a precedence table. They all allow for the tying of a
  *     `fixity: [[Fixity]]` to the parsers that inhabit the level, which will have type `fixity.Op[A, B]` for
  *     some `A` (the type of the layer below) and `B` (the type of this layer). This is the ''path-dependent''
  *     typing at work: the types of the parsers are only known when `fixity` itself is known. The different
  *     objects represent different inter-layer relationships: are they the same type? Use `Ops`; is the layer
  *     below a subtype of this layer? use `SOps`; or none of the above? Use `Gops`!
  *
  * @groupprio Fixities 50
  * @groupname Fixities Fixity Description
  * @groupdesc Fixities
  *     These all describe the fixities and associativities of operators at a given level of the precedence table.
  *     They are special because they each encode an `Op` type, which directs the type of the operators that are
  *     legal at the level (using ''path-dependent typing'').
  *
  * @groupprio Table 75
  * @groupname Table Precedence Table Datatypes
  * @groupdesc Table
  *     These are the parts that make up a precedence table (in particular, they are used for heterogeneous
  *     expression parsing, with the types of each layer of the table vary from one another). These are (mostly) not constructed
  *     directly, but are instead constructed via the use of the `Ops` builders or the `:+` and `+:` methods.
  */
package object expr
