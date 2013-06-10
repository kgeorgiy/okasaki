package info.kgeorgiy.okasaki

//  Exercise 2.1
object Suffixes {
  def apply[T](list: Seq[T]) = list.foldRight(List(List[T]()))((x, ss) => (x +: ss.head) +: ss)
}
