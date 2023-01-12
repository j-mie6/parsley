package parsley.token.errors

import parsley.Parsley
import parsley.XCompat.unused
import parsley.errors.combinator._

// This feels wrong? perhaps token is the wrong package
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}

private [parsley] trait ConfigImplUntyped {
    private [parsley] def apply[A](p: Parsley[A]): Parsley[A]
}

trait LabelConfig extends ConfigImplUntyped {
    private [parsley] def asExpectDesc: Option[ExpectDesc]
    private [parsley] def asExpectDesc(otherwise: String): Option[ExpectDesc]
    private [parsley] def asExpectItem(raw: String): Option[ExpectItem]
    private [parsley] def orElse(config: LabelConfig): LabelConfig
}

class Label(label: String) extends LabelConfig {
    require(label.nonEmpty, "label cannot be empty, use `Hidden` instead")
    private [parsley] final override def apply[A](p: Parsley[A]) = p.label(label)
    private [parsley] final override def asExpectDesc = Some(ExpectDesc(label))
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelConfig) = this
}
object Label {
    def apply(label: String): LabelConfig = new Label(label)
}

object Hidden extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p.hide
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(@unused otherwise: String) = asExpectDesc
    private [parsley] final override def asExpectItem(@unused raw: String) = asExpectDesc
    private [parsley] final override def orElse(config: LabelConfig) = this
}

object NotConfigured extends LabelConfig {
    private [parsley] final override def apply[A](p: Parsley[A]) = p
    private [parsley] final override def asExpectDesc = None
    private [parsley] final override def asExpectDesc(otherwise: String) = Some(ExpectDesc(otherwise))
    private [parsley] final override def asExpectItem(raw: String) = Some(ExpectRaw(raw))
    private [parsley] final override def orElse(config: LabelConfig) = config
}
