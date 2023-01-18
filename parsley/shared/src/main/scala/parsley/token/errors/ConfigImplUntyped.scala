package parsley.token.errors

import parsley.Parsley
import parsley.XCompat.unused
import parsley.errors.combinator._

// This feels wrong? perhaps token is the wrong package
// Because this is now used for Char, Sat, String to encode the label config...
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}

private [parsley] sealed trait ConfigImplUntyped {
    private [parsley] def apply[A](p: Parsley[A]): Parsley[A]
}

// TODO: move into internal?
// Relaxing Types
private [parsley] trait LabelOps {
    private [parsley] def asExpectDesc: Option[ExpectDesc]
    private [parsley] def asExpectDesc(otherwise: String): Option[ExpectDesc]
    private [parsley] def asExpectItem(raw: String): Option[ExpectItem]
    private [parsley] final def asExpectItem(raw: Char): Option[ExpectItem] = asExpectItem(s"$raw")
}

private [parsley] trait ExplainOps {
    // TODO: reason extraction, maybe tie into errors?
}

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

private final class Label private[errors]  (val label: String) extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.label(label)
    private [parsley] final override def asExpectDesc = Some(ExpectDesc(label))
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config match {
        case r: Reason => new LabelAndReason(label, r.reason)
        case lr: LabelAndReason => new LabelAndReason(label, lr.reason)
        case _ => this
    }
    private [parsley] final override def orElse(config: LabelConfig) = this
}
/** This object has a factory for configurations producing labels: if the empty string is provided, this equivalent to [[Hidden `Hidden`]].
  * @since 4.1.0
  * @group labels
  */
object Label {
    def apply(label: String): LabelConfig = if (label.isEmpty) Hidden else new Label(label)
}

/** This object configures labels by stating that it must be hidden.
  * @since 4.1.0
  * @group labels
  */
object Hidden extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.hide
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = this
    private [parsley] final override def orElse(config: LabelConfig) = this
}

private final class Reason private[errors]  (val reason: String) extends ExplainConfig {
    require(reason.nonEmpty, "reason cannot be empty, use `Label` instead")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.explain(reason)
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(otherwise: String) = Some(ExpectDesc(otherwise))
    private [parsley] final override def asExpectItem(raw: String) = Some(ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config match {
        case l: Label => new LabelAndReason(l.label, reason)
        case lr: LabelAndReason => new LabelAndReason(lr.label, reason)
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

private final class LabelAndReason private[errors] (val label: String, val reason: String) extends LabelWithExplainConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.label(label).explain(reason)
    private [parsley] final override def asExpectDesc = Some(ExpectDesc(label))
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = this
}
/** This object has a factory for configurations producing labels and reasons: if the empty label is provided, this equivalent to [[Hidden `Hidden`]] with no
  * reason; if the empty reason is provided this is equivalent to [[Label `Label`]].
  * @since 4.1.0
  * @group labels
  */
object LabelAndReason {
    def apply(label: String, reason: String): LabelWithExplainConfig = {
        if (label.isEmpty) Hidden
        else if (reason.nonEmpty) new LabelAndReason(label, reason)
        else new Label(label)
    }
}

/** This object specifies that no special labels or reasons should be generated, and default errors should be used instead.
  * @since 4.1.0
  * @group labels
  */
object NotConfigured extends LabelConfig with ExplainConfig with LabelWithExplainConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(otherwise: String) = Some(ExpectDesc(otherwise))
    private [parsley] final override def asExpectItem(raw: String) = Some(ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelWithExplainConfig) = config
    private [parsley] final override def orElse(config: LabelConfig) = config
}
