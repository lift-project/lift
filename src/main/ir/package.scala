package object ir {
  sealed trait Ordering
  final case object ByName extends Ordering
  final case object ByDeclarationOrder extends Ordering

  trait AddressSpace
}
