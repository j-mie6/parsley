package parsley.internal.machine

import parsley.state.Ref
import parsley.debug.Codec

private [parsley] case class RefCodec[A](val ref: Ref[A], val codec: Codec[A]) {
    import scala.util.Try
    
    private [parsley] def encodeRef(context: Context): String = {
        codec.encode(context.regs(ref.addr).asInstanceOf[A])
    }

    private [parsley] def updateRef(s: String, context: Context): Try[Unit] = {
        codec.decode(s).map(context.writeReg(ref.addr, _))
    }

}
