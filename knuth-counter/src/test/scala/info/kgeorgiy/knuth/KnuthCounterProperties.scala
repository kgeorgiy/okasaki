package info.kgeorgiy.knuth

import org.scalacheck.{Gen, Properties}

object KnuthCounterProperties extends Properties("KnuthCounter") {
  import org.scalacheck.Prop._

  val Bits = 10
  val Radix = 3

  val bitGen = Gen.choose(0, Bits - 1)
  val radixGen = Gen.choose(2, Radix)
  val digitsGen = Gen.containerOf[List, Int](bitGen)
  val plusMinusDigitsGen = Gen.containerOf[List, Int](Gen.choose(-Bits, Bits - 1))

  property("incSingleBit") = forAll(radixGen, bitGen){(radix, bit) => {
    radix.ensuring(2 <= radix)

    val counter = new KnuthCounter(radix)
    counter.inc(bit)
    counter.value == BigInt(radix).pow(bit)
  }}

  property("incMultiBit") = forAll(radixGen, digitsGen){(radix, digits) => radix >= 2 ==> {
    val counter = new KnuthCounter(radix)
    digits.foreach(counter.inc)
    counter.value === digits.foldLeft(BigInt(0))((v, bit) => v + BigInt(radix).pow(bit))
  }}

  property("decSingleBit") = forAll(radixGen, bitGen){(radix, bit) => radix >= 2 ==> {
    assert(2 <= radix, "radix=" + radix)

    val counter = new KnuthCounter(radix)
    counter.dec(bit)
    counter.value == -BigInt(radix).pow(bit)
  }}

  property("decMultiBit") = forAll(radixGen, digitsGen){(radix, digits) => radix >= 2 ==> {
    //    println(digits)
    val counter = new KnuthCounter(radix)
    digits.foreach(counter.dec)
    counter.value === digits.foldLeft(BigInt(0))((v, bit) => v - BigInt(radix).pow(bit))
  }}

  property("incThenDecMultiBit") = forAll(radixGen, digitsGen, digitsGen){(radix, incDigits, decDigits) => radix >= 2 ==> {
    //    println(incDigits)
    val counter = new KnuthCounter(radix)
    incDigits.foreach(counter.inc)
    decDigits.foreach(counter.dec)
    counter.value === decDigits.foldLeft(incDigits.foldLeft(BigInt(0))((v, bit) => v + BigInt(radix).pow(bit)))((v, bit) => v - BigInt(radix).pow(bit))
  }}

  property("incDecMultiBit") = forAll(radixGen, plusMinusDigitsGen){(radix, digits) => radix >= 2 ==> {
    val counter = new KnuthCounter(radix)
    digits.foreach(digit => if (digit >= 0) counter.inc(digit) else counter.dec(-digit - 1))
//    println(counter.value)
    counter.value === digits.foldLeft(BigInt(0))((v, digit) => v + (if (digit >= 0) BigInt(radix).pow(digit) else -BigInt(radix).pow(-digit - 1)))
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
