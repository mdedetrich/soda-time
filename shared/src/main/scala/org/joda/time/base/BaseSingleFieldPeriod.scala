package org.joda.time.base

import java.io.Serializable
import org.joda.time.DateTimeUtils
import org.joda.time.DurationFieldType
import org.joda.time.MutablePeriod
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial
import org.joda.time.ReadablePeriod
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.FieldUtils

object BaseSingleFieldPeriod {

  private val START_1972 = 2L * 365L * 86400L * 1000L

  def between(start: ReadableInstant,
              end: ReadableInstant,
              field: DurationFieldType): Int = {
    if (start == null || end == null) {
      throw new IllegalArgumentException(
        "ReadableInstant objects must not be null")
    }
    val chrono = DateTimeUtils.getInstantChronology(start)
    val amount =
      field.getField(chrono).getDifference(end.getMillis, start.getMillis)
    amount
  }

  def between(start: ReadablePartial,
              end: ReadablePartial,
              zeroInstance: ReadablePeriod): Int = {
    if (start == null || end == null) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must not be null")
    }
    if (start.size != end.size) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must have the same set of fields")
    }
    for (i <- 0 until start.size()
         if start.getFieldType(i) != end.getFieldType(i)) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must have the same set of fields")
    }
    if (DateTimeUtils.isContiguous(start) == false) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must be contiguous")
    }
    val chrono = DateTimeUtils.getChronology(start.getChronology).withUTC()
    val values = chrono.get(zeroInstance,
                            chrono.set(start, START_1972),
                            chrono.set(end, START_1972))
    values(0)
  }

  def standardPeriodIn(period: ReadablePeriod, millisPerUnit: Long): Int = {
    if (period == null) {
      return 0
    }
    val iso = ISOChronology.getInstanceUTC
    var duration = 0L
    for (i <- 0 until period.size) {
      val value = period.getValue(i)
      if (value != 0) {
        val field = period.getFieldType(i).getField(iso)
        if (field.isPrecise == false) {
          throw new IllegalArgumentException(
            "Cannot convert period to duration as " + field.getName +
              " is not precise in the period " +
              period)
        }
        duration = FieldUtils.safeAdd(
          duration,
          FieldUtils.safeMultiply(field.getUnitMillis, value))
      }
    }
    FieldUtils.safeToInt(duration / millisPerUnit)
  }
}

@SerialVersionUID(9386874258972L)
abstract class BaseSingleFieldPeriod protected (
    @volatile private var iPeriod: Int)
    extends ReadablePeriod()
    with Comparable[BaseSingleFieldPeriod]
    with Serializable {

  protected def getValue(): Int = iPeriod

  protected def setValue(value: Int) {
    iPeriod = value
  }

  def getFieldType(): DurationFieldType

  def getPeriodType(): PeriodType

  def size(): Int = 1

  def getFieldType(index: Int): DurationFieldType = {
    if (index != 0) {
      throw new IndexOutOfBoundsException(String.valueOf(index))
    }
    getFieldType
  }

  def getValue(index: Int): Int = {
    if (index != 0) {
      throw new IndexOutOfBoundsException(String.valueOf(index))
    }
    getValue
  }

  def get(`type`: DurationFieldType): Int = {
    if (`type` == getFieldType) {
      return getValue
    }
    0
  }

  def isSupported(`type`: DurationFieldType): Boolean =
    (`type` == getFieldType)

  def toPeriod(): Period = Period.ZERO.withFields(this)

  def toMutablePeriod(): MutablePeriod = {
    val period = new MutablePeriod()
    period.add(this)
    period
  }

  override def equals(period: Any): Boolean = {
    if (this == period) {
      return true
    }
    if (period.isInstanceOf[ReadablePeriod] == false) {
      return false
    }
    val other = period.asInstanceOf[ReadablePeriod]
    other.getPeriodType == getPeriodType && other.getValue(0) == getValue
  }

  override def hashCode(): Int = {
    var total = 17
    total = 27 * total + getValue
    total = 27 * total + getFieldType.hashCode
    total
  }

  def compareTo(other: BaseSingleFieldPeriod): Int = {
    if (other.getClass != getClass) {
      throw new ClassCastException(
        getClass + " cannot be compared to " + other.getClass)
    }
    val otherValue = other.getValue
    val thisValue = getValue
    if (thisValue > otherValue) {
      return 1
    }
    if (thisValue < otherValue) {
      return -1
    }
    0
  }
}
