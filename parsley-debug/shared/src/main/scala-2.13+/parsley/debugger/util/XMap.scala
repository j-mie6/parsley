/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

// This is used to give Scala 2.12 access to "removed" and "updated".
// In Scala 2.13 and up, this doesn't need any extra methods.
// Ideally, this would be done via scala-collection-compat, but that requires integrating ScalaFix.
private [parsley] trait XMap[K, +V] extends Map[K, V]
