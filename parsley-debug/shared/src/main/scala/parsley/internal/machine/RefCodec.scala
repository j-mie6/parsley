package parsley.internal.machine

import parsley.state.Ref
import parsley.debug.Codec

abstract class RefCodec {
    type A

    val ref: Ref[A]
    val codec: Codec[A]
}
