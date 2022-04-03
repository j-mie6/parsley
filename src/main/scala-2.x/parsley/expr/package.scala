package parsley

/** This package contains various functionality relating to the parsing of expressions, including precedence encoding combinators.
  *
  * @groupprio chains 0
  * @groupname chains Chain Combinators
  * @groupdesc chains
  *     These are all modules that contain the standalone chain combinators. These are useful for parsing the application
  *     of operators to values in many different configurations, including: prefix, postfix, infix (left and right associated),
  *     and mixed fixity. These combinators do have their uses, however they will be overshadowed by the `precedence` combinator,
  *     which allows for the combining of multiple levels of chaining in a clean and concise way.
  *
  * @groupprio precedence 10
  * @groupname precedence The Precedence Combinator
  * @groupdesc precedence
  *     This object contains `apply` methods which represent each of the different "shapes" of precedence combinator, which
  *     takes a description of the precedence relations between various operators and constructs a parser for them.
  *
  * @groupprio builders 20
  * @groupname builders Precedence Layer Builders
  * @groupdesc builders
  *     These are used to construct an individual layer of a precedence table. They all allow for the tying of a
  *     `fixity: [[Fixity]]` to the parsers that inhabit the level, which will have type `fixity.Op[A, B]` for
  *     some `A` (the type of the layer below) and `B` (the type of this layer). This is the ''path-dependent''
  *     typing at work: the types of the parsers are only known when `fixity` itself is known. The different
  *     objects represent different inter-layer relationships: are they the same type? Use `Ops`; is the layer
  *     below a subtype of this layer? use `SOps`; or none of the above? Use `Gops`!
  *
  * @groupprio fixities 50
  * @groupname fixities Fixity Description
  * @groupdesc fixities
  *     These all describe the fixities and associativities of operators at a given level of the precedence table.
  *     They are special because they each encode an `Op` type, which directs the type of the operators that are
  *     legal at the level (using ''path-dependent typing'').
  *
  * @groupprio table 75
  * @groupname table Precedence Table Datatypes
  * @groupdesc table
  *     These are the parts that make up a precedence table (in particular, they are used for heterogeneous
  *     expression parsing, with the types of each layer of the table vary from one another). These are (mostly) not constructed
  *     directly, but are instead constructed via the use of the `Ops` builders or the `:+` and `+:` methods.
  */
package object expr
