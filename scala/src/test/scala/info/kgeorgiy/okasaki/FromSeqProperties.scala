package info.kgeorgiy.okasaki

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Commands
import org.scalacheck.Arbitrary

class EqProperties(name: String) extends Properties(name) {
  implicit class Equalizer(value: Any) {
    def ===(value: Any) = {
      val equal = this.value == value
      if (!equal) {
        println(this.value + " =/= " + value)
      }
      equal
    }
  }
}

class FromSeqProperties[T[_] <: AnyRef](
  name: String,
  tested: T[Int],
  reference: T[Int],
  val fromSeq: (T[Int], Seq[Int]) => T[Int]
 ) extends EqProperties(name) {
  def test2(f: (T[Int], Int) => Any) = forAll{(xs: List[Int], x: Int) =>
    f(fromSeq(tested, xs), x) === f(fromSeq(reference, xs), x)
  }

  def test2const(p: (T[Int], Int) => Boolean, f: (T[Int], Int) => T[Int]) = forAll{(xs: List[Int], x: Int) =>
    val old: T[Int] = fromSeq(tested, xs)
    p(old, x) ==> {
      old.eq(f(old, x))
    }
  }

  def test1(f: (T[Int]) => Any) = test2((xs, x) => f(xs))

  abstract class IsoCommands extends Commands {
    case class State(tested: T[Int], reference: T[Int])

    implicit class Equalizer(value: Any) {
      def ===(value: Any) = {
        val equal = this.value == value
        if (!equal) {
          println(this.value + " =/= " + value)
        }
        equal
      }
    }

    def initialState() = {
      State(tested, reference)
    }

    abstract class Mutate[R](f: T[Int] => T[Int], g: T[Int] => R) extends Command {
      def run(s: State) = s match {case State(tested, reference) => (g(tested), g(reference))}
      def nextState(s: State) = s match {case State(tested, reference) => State(f(tested), f(reference))}
      postConditions += {case (_, _, (testedResult, referenceResult)) => testedResult === referenceResult}
    }

    def intGen[P](f : Int => P) = Arbitrary.arbitrary[Int].map(f)
  }
}
