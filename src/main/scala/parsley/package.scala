import parsley.internal.instructions.Context
import parsley.unsafe.FastRun

import scala.annotation.implicitAmbiguous
import scala.language.implicitConversions

package object parsley
{
    // $COVERAGE-OFF$
    // TODO: Remove
    // From shapeless library :)
    private [parsley] trait =!=[A, B]
    implicit def neq[A, B] : A =!= B = null
    @implicitAmbiguous("Must specify the type for get operation; S cannot be Nothing")
    implicit def neqAmbig1[A] : A =!= A = null
    implicit def neqAmbig2[A] : A =!= A = null
    // $COVERAGE-ON$
}
