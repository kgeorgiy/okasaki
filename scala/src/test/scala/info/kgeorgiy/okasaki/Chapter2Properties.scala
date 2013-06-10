package info.kgeorgiy.okasaki

import org.scalacheck.Prop._

object Chapter2Properties extends EqProperties("TreeSet") {
  include(SuffixesSpec)
  include(new OrderedSetProperties("TreeSet", TreeSet[Int]))
  include(new OrderedSetProperties("ShortInsert", TreeSetShortInsert[Int]))
  include(new OrderedSetCutProperties("CutInsert", TreeSetCutInsert[Int]))
  include(new OrderedSetCutProperties("ShortCutInsert", TreeSetShortCutInsert[Int]))

  property("complete") = forAll{(s: Int, v: String) => (s > 0) ==> {
    new TreeSet(TreeUtil.complete(s % 10, v), Ordering.String).toSeq === Seq.fill((1 << s % 10) - 1)(v)
  }}

  property("create") = forAll{(s: Int, v: String) => (s > 0) ==> {
    new TreeSet(TreeUtil.create(s % 100000, v), Ordering.String).toSeq === Seq.fill(s % 100000)(v)
  }}
}
