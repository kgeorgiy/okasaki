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
    fix(pointer(index))
    updateDigit(index, delta)
    fix(pointer(index))
  }

  private def fix(index: Int) {
    if (digit(index) == radix) {
      updateDigit(index + 1, 1)
      set(index, 0)
    }
    if (digit(index) == -1) {
      updateDigit(index + 1, -1)
      set(index, radix - 1)
    }
  }

  private def updateDigit(index: Int, delta: Int) {
    set(index, digit(index) + delta)
    val red = if (delta > 0) radix else -1
    pointers(index) = if (digit(index) == red) index else pointer(index + 1)
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
