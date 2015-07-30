package org.joda.time

abstract class DurationField extends Comparable[DurationField] {

  def getType(): DurationFieldType

  def getName(): String

  def isSupported(): Boolean

  def isPrecise(): Boolean

  def getUnitMillis(): Long

  def getValue(duration: Long): Int

  def getValueAsLong(duration: Long): Long

  def getValue(duration: Long, instant: Long): Int

  def getValueAsLong(duration: Long, instant: Long): Long

  def getMillis(value: Int): Long

  def getMillis(value: Long): Long

  def getMillis(value: Int, instant: Long): Long

  def getMillis(value: Long, instant: Long): Long

  def add(instant: Long, value: Int): Long

  def add(instant: Long, value: Long): Long

  def subtract(instant: Long, value: Int): Long = {
    if (value == Integer.MIN_VALUE) {
      return subtract(instant, value.toLong)
    }
    add(instant, -value)
  }

  def subtract(instant: Long, value: Long): Long = {
    if (value == Long.MinValue) {
      throw new ArithmeticException("Long.MIN_VALUE cannot be negated")
    }
    add(instant, -value)
  }

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long

  override def toString(): String
}
