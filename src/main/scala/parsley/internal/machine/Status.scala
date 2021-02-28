package parsley.internal.machine

private [machine] sealed abstract class Status
private [machine] case object Good extends Status
private [machine] case object Recover extends Status
private [machine] case object Failed extends Status