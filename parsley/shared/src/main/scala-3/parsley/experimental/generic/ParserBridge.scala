package parsley
package experimental.generic

import scala.quoted.*
import scala.deriving.*

/*
Problem space:
    * What are the shapes of the typeclass
    * How are error bridges incorporated in (annotation?)
    * How do multi-argument typeclasses work with macros?
*/

// looks like typeclasses cannot have more than one type parameter
//trait ParserBridge1[T1, R]

// class class Foo(x: Int, y: Int) derives ParserBridge2[Int, Int].Type

/*class ParserBridge1[A] {
    trait Impl[T] {
        def apply(p: Parsley[A]): Parsley[T]
    }
}
object ParserBridge1 {
    def bridgeImpl[A: Type, T: Type](using Quotes): Expr[ParserBridge1[A]#Impl[T]] = ???
}

inline def derived[A, T]: ParserBridge1[A]#Impl[T] = ${ParserBridge1.bridgeImpl[A, T]}*/



inline transparent def bridge1[T](using mirror: Mirror.ProductOf[T]) = ${bridge1Impl[T]('mirror)}
def bridge1Impl[T: Type](mirror: Expr[Mirror.ProductOf[T]])(using Quotes): Expr[?] = {
    
    '{???}
}

/*
data Foo = Foo Int  Pos String

$(deriveLiftedConstructors "mk" ['Foo])
-- mkFoo :: Parsec Int -> Parsec String -> Parsec Foo
-- mkFoo pi ps = f <$> pos <*> pi <*> ps
--   where f pos i s = Foo i pos s
*/
