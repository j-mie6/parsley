/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding

private [parsley] object Sign {
    private [parsley] sealed trait SignType {
        type resultType
    }
    private [parsley] case object DoubleType extends SignType {
        override type resultType = BigDecimal
    }
    private [parsley] case object IntType extends SignType {
        override type resultType = BigInt
    }
}
