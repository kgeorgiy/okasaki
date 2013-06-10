package info.kgeorgiy.okasaki

import org.scalacheck.Properties
import org.scalacheck.Prop._

object SuffixesSpec extends Properties("Suffixes") {
  property("length") = forAll((xs : List[Int]) => Suffixes(xs).length == xs.length + 1)

  property("contents") = forAll((xs : List[Int], i: Int) => (i >= 0) ==> {
    Suffixes(xs)(i % (xs.length + 1)) == xs.drop(i % (xs.length + 1))
  })

  property("linear") = forAll((xs : List[Int], i: Int) => (i >= 0 && xs.length > 0) ==> {
    val suffixes = Suffixes(xs)
    suffixes(i % xs.length).tail.eq(suffixes(i % xs.length + 1))
  })
}
