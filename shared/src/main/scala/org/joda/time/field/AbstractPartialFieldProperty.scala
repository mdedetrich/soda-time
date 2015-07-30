package org.joda.time.field

import java.util.Locale
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial

abstract class AbstractPartialFieldProperty protected () {

  def getField(): DateTimeField

  def getFieldType(): DateTimeFieldType = getField.getType

  def getName(): String = getField.getName

  protected def getReadablePartial(): ReadablePartial

  def get(): Int

  def getAsString(): String = Integer.toString(get)

  def getAsText(): String = getAsText(null)

  def getAsText(locale: Locale): String = {
    getField.getAsText(getReadablePartial, get, locale)
  }

  def getAsShortText(): String = getAsShortText(null)

  def getAsShortText(locale: Locale): String = {
    getField.getAsShortText(getReadablePartial, get, locale)
  }

  def getDurationField(): DurationField = getField.getDurationField

  def getRangeDurationField(): DurationField = getField.getRangeDurationField

  def getMinimumValueOverall(): Int = getField.getMinimumValue

  def getMinimumValue(): Int = {
    getField.getMinimumValue(getReadablePartial)
  }

  def getMaximumValueOverall(): Int = getField.getMaximumValue

  def getMaximumValue(): Int = {
    getField.getMaximumValue(getReadablePartial)
  }

  def getMaximumTextLength(locale: Locale): Int = getField.getMaximumTextLength(locale)

  def getMaximumShortTextLength(locale: Locale): Int = {
    getField.getMaximumShortTextLength(locale)
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
      throw new IllegalArgumentException("The instant must not be null")
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
    if (`object`.isInstanceOf[AbstractPartialFieldProperty] ==
      false) {
      return false
    }
    val other = `object`.asInstanceOf[AbstractPartialFieldProperty]
    get == other.get && getFieldType == other.getFieldType &&
      FieldUtils.==(getReadablePartial.getChronology, other.getReadablePartial.getChronology)
  }

  override def hashCode(): Int = {
    var hash = 19
    hash = 13 * hash + get
    hash = 13 * hash + getFieldType.hashCode
    hash = 13 * hash + getReadablePartial.getChronology.hashCode
    hash
  }

  override def toString(): String = "Property[" + getName + "]"
}
