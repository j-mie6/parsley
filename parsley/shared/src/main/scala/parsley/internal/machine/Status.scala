/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

private [machine] sealed abstract class Status
private [machine] case object Good extends Status
private [machine] case object Recover extends Status
private [machine] case object Finished extends Status
private [machine] case object Failed extends Status
