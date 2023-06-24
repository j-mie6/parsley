/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods

// This feels wrong? perhaps token is the wrong package
// Because this is now used for Char, Sat, String to encode the label config...
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}

import org.typelevel.scalaccompat.annotation.unused

private [parsley] sealed trait ConfigImplUntyped {
    private [parsley] def apply[A](p: Parsley[A]): Parsley[A]
}

// TODO: move into internal?
// Relaxing Types
private [parsley] trait LabelOps {
    private [parsley] def asExpectDescs: Iterable[ExpectDesc]
    private [parsley] def asExpectDescs(otherwise: String): Iterable[ExpectDesc]
    private [parsley] def asExpectItems(raw: String): Iterable[ExpectItem]
    private [parsley] final def asExpectItems(raw: Char): Iterable[ExpectItem] = asExpectItems(s"$raw")
}

// TODO: reason extraction, maybe tie into errors?
private [parsley] trait ExplainOps


// Constraining Types
/** This type can be used to configure ''both'' errors that make labels and those that make reasons.
  * @since 4.1.0
  * @group labels
  */
trait LabelWithExplainConfig extends ConfigImplUntyped with LabelOps with ExplainOps {
    private [parsley] def orElse(other: LabelWithExplainConfig): LabelWithExplainConfig
}
/** This type can be used to configure errors that make labels.
  * @since 4.1.0
  * @group labels
  */
trait LabelConfig extends LabelWithExplainConfig {
    private [parsley] def orElse(other: LabelConfig): LabelConfig
}
/** This type can be used to configure errors that make reasons.
  * @since 4.1.0
  * @group labels
  */
trait ExplainConfig extends LabelWithExplainConfig

private [errors] final class Label private[errors] (val labels: Seq[String]) extends LabelConfig {
    require(labels.forall(_.nonEmpty), "labels cannot be empty strings")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.labels(labels: _*)
    private [parsley] final override def asExpectDescs: Iterable[ExpectDesc] = labels.map(new ExpectDesc(_))
    private [parsley] final override def asExpectDescs(@unused otherwise: String) = asExpectDescs
    private [parsley] final override def asExpectItems(@unused raw: String) = asExpectDescs
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config match {
        case r: Reason => new LabelAndReason(labels, r.reason)
        case lr: LabelAndReason => new LabelAndReason(labels, lr.reason)
        case _ => this
    }
    private [parsley] final override def orElse(config: LabelConfig) = this
}
/** This object has a factory for configurations producing labels: if the empty string is provided, this equivalent to [[Hidden `Hidden`]].
  * @since 4.1.0
  * @group labels
  */
object Label {
    def apply(label: String): LabelConfig = if (label.isEmpty) Hidden else new Label(Seq(label))
    private [parsley] def apply(labels: String*) = if (labels.isEmpty) Hidden else new Label(labels)
}

/** This object configures labels by stating that it must be hidden.
  * @since 4.1.0
  * @group labels
  */
object Hidden extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.hide
    private [parsley] final override def asExpectDescs = None
    private [parsley] final override def asExpectDescs(@unused otherwise: String) = asExpectDescs
    private [parsley] final override def asExpectItems(@unused raw: String) = asExpectDescs
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = this
    private [parsley] final override def orElse(config: LabelConfig) = this
}

private [errors] final class Reason private[errors]  (val reason: String) extends ExplainConfig {
    require(reason.nonEmpty, "reasons cannot be empty strings")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.explain(reason)
    private [parsley] final override def asExpectDescs = None
    private [parsley] final override def asExpectDescs(otherwise: String) = Some(new ExpectDesc(otherwise))
    private [parsley] final override def asExpectItems(raw: String) = Some(new ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config match {
        case l: Label => new LabelAndReason(l.labels, reason)
        case lr: LabelAndReason => new LabelAndReason(lr.labels, reason)
        case _ => this
    }
}
/** This object has a factory for configurations producing reasons: if the empty string is provided, this equivalent to [[NotConfigured `NotConfigured`]].
  * @since 4.1.0
  * @group labels
  */
object Reason {
    def apply(reason: String): ExplainConfig = if (reason.nonEmpty) new Reason(reason) else NotConfigured
}

private [errors] final class LabelAndReason private[errors] (val labels: Seq[String], val reason: String) extends LabelWithExplainConfig {
    require(reason.nonEmpty, "reason cannot be empty strings, use `Label` instead")
    require(labels.forall(_.nonEmpty), "labels cannot be empty strings")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.labels(labels: _*).explain(reason)
    private [parsley] final override def asExpectDescs = labels.map(new ExpectDesc(_))
    private [parsley] final override def asExpectDescs(@unused otherwise: String) = asExpectDescs
    private [parsley] final override def asExpectItems(@unused raw: String) = asExpectDescs
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = this
}
/** This object has a factory for configurations producing labels and reasons: if the empty label is provided, this equivalent to [[Hidden `Hidden`]] with no
  * reason; if the empty reason is provided this is equivalent to [[Label$ `Label`]].
  * @since 4.1.0
  * @group labels
  */
object LabelAndReason {
    def apply(label: String, reason: String): LabelWithExplainConfig = {
        if (label.isEmpty) Hidden
        else if (reason.nonEmpty) new LabelAndReason(Seq(label), reason)
        else new Label(Seq(label))
    }
}

/** This object specifies that no special labels or reasons should be generated, and default errors should be used instead.
  * @since 4.1.0
  * @group labels
  */
object NotConfigured extends LabelConfig with ExplainConfig with LabelWithExplainConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p
    private [parsley] final override def asExpectDescs = None
    private [parsley] final override def asExpectDescs(otherwise: String) = Some(new ExpectDesc(otherwise))
    private [parsley] final override def asExpectItems(raw: String) = Some(new ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config
    private [parsley] final override def orElse(config: LabelConfig) = config
}
