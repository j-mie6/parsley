package parsley

package object internal {
    private [internal] type UnsafeOption[A >: Null] = A
}