package parsley.token.errors

import parsley.Parsley
import parsley.XCompat.unused
import parsley.errors.combinator._

// This feels wrong? perhaps token is the wrong package
// Because this is now used for Char, Sat, String to encode the label config...
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}

private [parsley] trait ConfigImplUntyped {
    private [parsley] def apply[A](p: Parsley[A]): Parsley[A]
}

// TODO: move into internal!
// Relaxing Types
private [parsley] trait LabelOps {
    private [parsley] def asExpectDesc: Option[ExpectDesc]
    private [parsley] def asExpectDesc(otherwise: String): Option[ExpectDesc]
    private [parsley] def asExpectItem(raw: String): Option[ExpectItem]
    private [parsley] final def asExpectItem(raw: Char): Option[ExpectItem] = asExpectItem(s"$raw")
    private [parsley] def orElse(config: LabelConfig): LabelConfig
}

private [parsley] trait ExplainOps {
    // TODO: reason extraction, maybe tie into errors?
}

// Constraining Types
trait LabelWithExplainConfig extends ConfigImplUntyped with LabelOps with ExplainOps
trait LabelConfig extends LabelWithExplainConfig
trait ExplainConfig extends LabelWithExplainConfig

class Label(label: String) extends LabelConfig {
    require(label.nonEmpty, "label cannot be empty, use `Hidden` instead")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.label(label)
    private [parsley] final override def asExpectDesc = Some(ExpectDesc(label))
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelConfig) = this
}
object Label {
    def apply(label: String): LabelConfig = if (label.isEmpty) Hidden else new Label(label)
}

object Hidden extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.hide
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelConfig) = this
}

class Reason(reason: String) extends ExplainConfig {
    require(reason.nonEmpty, "reason cannot be empty, use `NotConfigured` instead")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.explain(reason)
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(otherwise: String) = Some(ExpectDesc(otherwise))
    private [parsley] final override def asExpectItem(raw: String) = Some(ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelConfig) = config
}

class LabelAndReason(label: String, reason: String) extends LabelWithExplainConfig {
    require(label.nonEmpty, "label cannot be empty, use `Hidden` instead")
    require(reason.nonEmpty, "label cannot be empty, use `Label` instead")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.label(label).explain(reason)
    private [parsley] final override def asExpectDesc = Some(ExpectDesc(label))
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelConfig) = Label(label) // FIXME:
}
object LabelAndReason {
    def apply(label: String, reason: String): LabelWithExplainConfig = if (label.isEmpty) Hidden else new LabelAndReason(label, reason)
}

object NotConfigured extends LabelConfig with ExplainConfig with LabelWithExplainConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(otherwise: String) = Some(ExpectDesc(otherwise))
    private [parsley] final override def asExpectItem(raw: String) = Some(ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelConfig) = config
}
