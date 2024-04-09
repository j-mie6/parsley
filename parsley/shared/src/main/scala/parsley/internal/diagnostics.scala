/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.diagnostics

import parsley.exceptions.{CorruptedReferenceException, ParsleyException}

private [parsley] object UserException {
    def unapply(e: Throwable): Option[Throwable] = e match {
        case _: ParsleyException => None
        case e if userStackTrace(e.getStackTrace) =>
            e.setStackTrace(pruneParsley(e.getStackTrace))
            Some(e)
        case _ => None
    }

    def userStackTrace(e: Array[StackTraceElement]) = e.view.takeWhile(!_.getClassName.startsWith("parsley.internal")).exists { ste =>
        !ste.getClassName.startsWith("scala") || !ste.getClassName.startsWith("java")
    }
    def pruneParsley(e: Array[StackTraceElement]): Array[StackTraceElement] = {
        val (userBits, parsleyTrace) = e.span(!_.getClassName.startsWith("parsley.internal"))
        userBits ++ parsleyTrace.dropWhile(_.getClassName.startsWith("parsley.internal"))
    }
}

private [parsley] object RegisterOutOfBoundsException {
    def unapply(e: Throwable): Option[Throwable] = e match {
        case e: ArrayIndexOutOfBoundsException => e.getStackTrace.headOption.collect {
            // this exception was thrown plainly during the execution of an instruction
            // only register arrays are accessed raw like this: therefore it must be an
            // out of bounds register.
            case ste if ste.getMethodName == "apply"
                     && ste.getClassName.startsWith("parsley.internal.machine.instructions") =>
                new CorruptedReferenceException
        }
        case _ => None
    }
}
