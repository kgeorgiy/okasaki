package info.kgeorgiy.knuth

import scala.collection.mutable.ArrayBuffer

class KnuthCounter(val radix: Int)  {
  private val digits = new ArrayBuffer[Int]()
  private val pointers = new ArrayBuffer[Int]()

  def value = digits.foldLeft((BigInt(0), BigInt(1)))((sp, digit) => (sp._1 + digit * sp._2, sp._2 * radix))._1

  def inc(index: Int) {
    update(index, 1)
  }

  def dec(index: Int) {
    update(index, -1)
  }

  private def update(index: Int, delta: Int) {
    fix(index)
    fix(pointer(index))
    updateDigit(index, delta)
    //    set(index, digit(index) + 1)
    fix(index)
    fix(pointer(index))
    //    println(index + " " + digits + " " + pointers + " " + value)
  }

  private def fix(index: Int) {
    if (Math.abs(digit(index)) == radix) {
      updateDigit(index + 1, if (digit(index) > 0) 1 else -1)
      set(index, 0)
    }
  }

  private def updateDigit(index: Int, delta: Int) {
    set(index, digit(index) + delta)
    if (digit(index) == delta * (radix - 1)) {
      pointers(index) = if (digit(index + 1) == delta * radix) index + 1 else pointer(index + 1)
    }
  }

  private def set(index: Int, digit: Int) {
    while (digits.length <= index) {
      digits += 0
      pointers += 0
    }
    assert(-radix <= digit && digit <= radix, "digit=" + digit)
    digits(index) = digit
  }

  private def digit(index: Int) = {
    if (digits.length <= index) 0 else digits(index)
  }

  private def pointer(index: Int) = {
    if (digits.length <= index) 0 else pointers(index)
  }
}
