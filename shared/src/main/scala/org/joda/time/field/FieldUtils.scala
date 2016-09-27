package org.joda.time.field

import org.joda.time.{
  DateTimeField,
  DateTimeFieldType,
  IllegalFieldValueException
}

object FieldUtils {

  def safeNegate(value: Int): Int = {
    if (value == Integer.MIN_VALUE) {
      throw new ArithmeticException("Integer.MIN_VALUE cannot be negated")
    }
    -value
  }

  def safeAdd(val1: Int, val2: Int): Int = {
    val sum = val1 + val2
    if ((val1 ^ sum) < 0 && (val1 ^ val2) >= 0) {
      throw new ArithmeticException(
        "The calculation caused an overflow: " + val1 + " + " +
          val2)
    }
    sum
  }

  def safeAdd(val1: Long, val2: Long): Long = {
    val sum = val1 + val2
    if ((val1 ^ sum) < 0 && (val1 ^ val2) >= 0) {
      throw new ArithmeticException(
        "The calculation caused an overflow: " + val1 + " + " +
          val2)
    }
    sum
  }

  def safeSubtract(val1: Long, val2: Long): Long = {
    val diff = val1 - val2
    if ((val1 ^ diff) < 0 && (val1 ^ val2) < 0) {
      throw new ArithmeticException(
        "The calculation caused an overflow: " + val1 + " - " +
          val2)
    }
    diff
  }

  def safeMultiply(val1: Int, val2: Int): Int = {
    val total = val1.toLong * val2.toLong
    if (total < Integer.MIN_VALUE || total > Integer.MAX_VALUE) {
      throw new ArithmeticException(
        "Multiplication overflows an int: " + val1 + " * " + val2)
    }
    total.toInt
  }

  def safeMultiply(val1: Long, val2: Int): Long = val2 match {
    case -1 =>
      if (val1 == Long.MinValue) {
        throw new ArithmeticException(
          "Multiplication overflows a long: " + val1 + " * " + val2)
      }
      -val1

    case 0 => 0L
    case 1 => val1
  }

  def safeMultiply(val1: Long, val2: Long): Long = {
    if (val2 == 1) {
      return val1
    }
    if (val1 == 1) {
      return val2
    }
    if (val1 == 0 || val2 == 0) {
      return 0
    }
    val total = val1 * val2
    if (total / val2 != val1 || val1 == Long.MinValue && val2 == -1 ||
        val2 == Long.MinValue && val1 == -1) {
      throw new ArithmeticException(
        "Multiplication overflows a long: " + val1 + " * " + val2)
    }
    total
  }

  def safeDivide(dividend: Long, divisor: Long): Long = {
    if (dividend == Long.MinValue && divisor == -1L) {
      throw new ArithmeticException(
        "Multiplication overflows a long: " + dividend + " / " +
          divisor)
    }
    dividend / divisor
  }

  def safeToInt(value: Long): Int = {
    if (Integer.MIN_VALUE <= value && value <= Integer.MAX_VALUE) {
      return value.toInt
    }
    throw new ArithmeticException("Value cannot fit in an int: " + value)
  }

  def safeMultiplyToInt(val1: Long, val2: Long): Int = {
    val `val` = FieldUtils.safeMultiply(val1, val2)
    FieldUtils.safeToInt(`val`)
  }

  def verifyValueBounds(field: DateTimeField,
                        value: Int,
                        lowerBound: Int,
                        upperBound: Int) {
    if ((value < lowerBound) || (value > upperBound)) {
      throw IllegalFieldValueException.create(field.getType,
                                              Integer.valueOf(value),
                                              Integer.valueOf(lowerBound),
                                              Integer.valueOf(upperBound))
    }
  }

  def verifyValueBounds(fieldType: DateTimeFieldType,
                        value: Int,
                        lowerBound: Int,
                        upperBound: Int) {
    if ((value < lowerBound) || (value > upperBound)) {
      throw IllegalFieldValueException.create(fieldType,
                                              Integer.valueOf(value),
                                              Integer.valueOf(lowerBound),
                                              Integer.valueOf(upperBound))
    }
  }

  def verifyValueBounds(fieldName: String,
                        value: Int,
                        lowerBound: Int,
                        upperBound: Int) {
    if ((value < lowerBound) || (value > upperBound)) {
      throw IllegalFieldValueException.create(fieldName,
                                              Integer.valueOf(value),
                                              Integer.valueOf(lowerBound),
                                              Integer.valueOf(upperBound))
    }
  }

  def getWrappedValue(currentValue: Int,
                      wrapValue: Int,
                      minValue: Int,
                      maxValue: Int): Int = {
    getWrappedValue(currentValue + wrapValue, minValue, maxValue)
  }

  def getWrappedValue(value: Int, minValue: Int, maxValue: Int): Int = {
    var _value: Int = value
    if (minValue >= maxValue) {
      throw new IllegalArgumentException("MIN > MAX")
    }
    val wrapRange = maxValue - minValue + 1
    _value -= minValue
    if (_value >= 0) {
      return (_value % wrapRange) + minValue
    }
    val remByRange = (-_value) % wrapRange
    if (remByRange == 0) {
      return 0 + minValue
    }
    (wrapRange - remByRange) + minValue
  }

  def equals(object1: AnyRef, object2: AnyRef): Boolean = {
    if (object1 == object2) {
      return true
    }
    if (object1 == null || object2 == null) {
      return false
    }
    object1 == object2
  }
}
