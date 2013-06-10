package info.kgeorgiy.okasaki

import org.scalacheck.Gen
import org.scalacheck.Prop._
import scala.collection.immutable.SortedSet
import scala.math.Ordering

class OrderedSetProperties(name: String, empty: OrderedSet[Int]) extends FromSeqProperties[OrderedSet](
  name,
  empty,
  new ReferenceSortedSet[Int](SortedSet.empty),
  (empty, xs) => xs.foldLeft(empty)(_ + _)
) {
  property("fromSeq_toSeq") = test1(_.toSeq)
  property("isEmpty") = test1(_.toSeq)
  property("size") = test1(_.size)
  property("member") = test2(_(_))
  property("insert") = test2(_ + _ toSeq)

  property("random") = new IsoCommands() {
    case class Insert(value: Int) extends Mutate(_ + value, _.toSeq)
    case class Member(value: Int) extends Mutate(x => x, _(value))
    def genCommand(s: State) = Gen.oneOf(intGen(Insert), intGen(Member))
  }
}

class OrderedSetCutProperties(name: String, empty: OrderedSet[Int]) extends OrderedSetProperties(name, empty) {
  property("insert_existing") = test2const(_(_), _ + _)
}

class ReferenceSortedSet[T](elements: SortedSet[T]) extends OrderedSet[T] {
  def ordering = elements.ordering
  def empty = new ReferenceSortedSet(elements.empty)
  def isEmpty = elements.isEmpty
  def size = elements.size
  def apply(value: T) = elements.apply(value)
  def +(value: T) = new ReferenceSortedSet[T](elements + value)
  def toSeq = elements.toSeq
}
