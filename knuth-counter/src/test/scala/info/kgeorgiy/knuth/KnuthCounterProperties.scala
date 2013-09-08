package info.kgeorgiy.knuth

import org.scalacheck.{Gen, Properties}

object KnuthCounterProperties extends Properties("KnuthCounter") {
  import org.scalacheck.Prop._

  val Bits = 60
  val Radix = 5

  val bitGen = Gen.choose(0, Bits - 1)
  val radixGen = Gen.choose(2, Radix)
  val digitsGen = Gen.containerOf[List, Int](bitGen)

  property("singleBit") = forAll(radixGen, bitGen){(radix, bit) => {
    radix.ensuring(2 <= radix)

    val counter = new KnuthCounter(radix)
    counter.inc(bit)
    counter.value == BigInt(radix).pow(bit)
  }}

  property("multiBit") = forAll(radixGen, digitsGen){(radix, digits) => radix >= 2 ==> {
//    println("radix=" + radix + ", digits = " + digits)
    val counter = new KnuthCounter(radix)
    digits.foreach(counter.inc)
//    println(counter.value)

    counter.value === digits.foldLeft(BigInt(0))((v, bit) => v + BigInt(radix).pow(bit))
  }}

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
