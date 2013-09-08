package info.kgeorgiy.knuth

import scala.collection.mutable.ArrayBuffer

class KnuthCounter(val radix: Int)  {
  private val digits = new ArrayBuffer[Int]()
  private val pointers = new ArrayBuffer[Int]()

  def value = digits.foldLeft((BigInt(0), BigInt(1)))((sp, digit) => (sp._1 + digit * sp._2, sp._2 * radix))._1

  def inc(index: Int) {
    fix(index)
    fix(pointer(index))
    incDigit(index - 1)
//    set(index, digit(index) + 1)
    fix(index)
    fix(pointer(index))
//    println(index + " " + digits + " " + pointers + " " + value)
  }


  def fix(index: Int) {
    if (digit(index) == radix) {
      incDigit(index)
      set(index, 0)
    }
  }


  def incDigit(index: Int) {
    set(index + 1, digit(index + 1) + 1)
    if (digit(index + 1) == radix - 1) {
      pointers(index + 1) = if (digit(index + 2) == radix) index + 2 else pointer(index + 2)
    } else {
//      pointers(index + 1) = pointer(index + 2)
    }
  }

  private def set(index: Int, digit: Int) {
    while (digits.length <= index) {
      digits += 0
      pointers += 0
    }
    digits(index) = digit.ensuring(0 <= _).ensuring(_ <= radix)
  }

  private def digit(index: Int) = {
    if (digits.length <= index) 0 else digits(index)
  }

  private def pointer(index: Int) = {
    if (digits.length <= index) 0 else pointers(index)
  }
}
