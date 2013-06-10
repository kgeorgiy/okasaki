package info.kgeorgiy.okasaki

trait OrderedSet[T] {
  def ordering: Ordering[T]
  def empty: OrderedSet[T]
  def isEmpty: Boolean
  def size: Int
  def apply(value: T): Boolean
  def +(value: T): OrderedSet[T]
  def toSeq : Seq[T]
}
