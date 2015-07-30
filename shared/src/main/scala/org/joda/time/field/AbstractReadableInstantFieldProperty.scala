package org.joda.time.field

import java.io.Serializable
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DurationField
import org.joda.time.Interval
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial

@SerialVersionUID(1971226328211649661L)
abstract class AbstractReadableInstantFieldProperty extends Serializable() {

  def getField(): DateTimeField

  def getFieldType(): DateTimeFieldType = getField.getType

  def getName(): String = getField.getName

  protected def getMillis(): Long

  protected def getChronology(): Chronology = {
    throw new UnsupportedOperationException("The method getChronology() was added in v1.4 and needs " +
      "to be implemented by subclasses of AbstractReadableInstantFieldProperty")
  }

  def get(): Int = getField.get(getMillis)

  def getAsString(): String = Integer.toString(get)

  def getAsText(): String = getAsText(null)

  def getAsText(locale: Locale): String = getField.getAsText(getMillis, locale)

  def getAsShortText(): String = getAsShortText(null)

  def getAsShortText(locale: Locale): String = {
    getField.getAsShortText(getMillis, locale)
  }

  def getDifference(instant: ReadableInstant): Int = {
    if (instant == null) {
      return getField.getDifference(getMillis, DateTimeUtils.currentTimeMillis())
    }
    getField.getDifference(getMillis, instant.getMillis)
  }

  def getDifferenceAsLong(instant: ReadableInstant): Long = {
    if (instant == null) {
      return getField.getDifferenceAsLong(getMillis, DateTimeUtils.currentTimeMillis())
    }
    getField.getDifferenceAsLong(getMillis, instant.getMillis)
  }

  def getDurationField(): DurationField = getField.getDurationField

  def getRangeDurationField(): DurationField = getField.getRangeDurationField

  def isLeap(): Boolean = getField.isLeap(getMillis)

  def getLeapAmount(): Int = getField.getLeapAmount(getMillis)

  def getLeapDurationField(): DurationField = getField.getLeapDurationField

  def getMinimumValueOverall(): Int = getField.getMinimumValue

  def getMinimumValue(): Int = getField.getMinimumValue(getMillis)

  def getMaximumValueOverall(): Int = getField.getMaximumValue

  def getMaximumValue(): Int = getField.getMaximumValue(getMillis)

  def getMaximumTextLength(locale: Locale): Int = getField.getMaximumTextLength(locale)

  def getMaximumShortTextLength(locale: Locale): Int = {
    getField.getMaximumShortTextLength(locale)
  }

  def remainder(): Long = getField.remainder(getMillis)

  def toInterval(): Interval = {
    val field = getField
    val start = field.roundFloor(getMillis)
    val end = field.add(start, 1)
    val interval = new Interval(start, end)
    interval
  }

  def compareTo(instant: ReadableInstant): Int = {
    if (instant == null) {
      throw new IllegalArgumentException("The instant must not be null")
    }
    val thisValue = get
    val otherValue = instant.get(getFieldType)
    if (thisValue < otherValue) {
      -1
    } else if (thisValue > otherValue) {
      1
    } else {
      0
    }
  }

  def compareTo(partial: ReadablePartial): Int = {
    if (partial == null) {
      throw new IllegalArgumentException("The partial must not be null")
    }
    val thisValue = get
    val otherValue = partial.get(getFieldType)
    if (thisValue < otherValue) {
      -1
    } else if (thisValue > otherValue) {
      1
    } else {
      0
    }
  }

  override def equals(`object`: Any): Boolean = {
    if (this == `object`) {
      return true
    }
    if (`object`.isInstanceOf[AbstractReadableInstantFieldProperty] ==
      false) {
      return false
    }
    val other = `object`.asInstanceOf[AbstractReadableInstantFieldProperty]
    get == other.get && getFieldType == other.getFieldType &&
      FieldUtils.==(getChronology, other.getChronology)
  }

  override def hashCode(): Int = {
    get * 17 + getFieldType.hashCode + getChronology.hashCode
  }

  override def toString(): String = "Property[" + getName + "]"
}
