package org.joda.time.field

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField

@SerialVersionUID(-5586801265774496376L)
class PreciseDateTimeField(`type`: DateTimeFieldType,
                           unit: DurationField,
                           private val range: DurationField)
    extends PreciseDurationDateTimeField(`type`, unit) {

  private var iRangeField: DurationField = null
  private var iRange: Int = _

  if (!range.isPrecise) {
    throw new IllegalArgumentException("Range duration field must be precise")
  }

  val rangeMillis = range.getUnitMillis

  iRange = (rangeMillis / getUnitMillis).toInt

  if (iRange < 2) {
    throw new IllegalArgumentException(
      "The effective range must be at least 2")
  }

  iRangeField = range

  def get(instant: Long): Int = {
    if (instant >= 0) {
      ((instant / getUnitMillis) % iRange).toInt
    } else {
      iRange - 1 + (((instant + 1) / getUnitMillis) % iRange).toInt
    }
  }

  override def addWrapField(instant: Long, amount: Int): Long = {
    val thisValue = get(instant)
    val wrappedValue = FieldUtils
      .getWrappedValue(thisValue, amount, getMinimumValue, getMaximumValue)
    instant + (wrappedValue - thisValue) * getUnitMillis
  }

  override def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, getMinimumValue, getMaximumValue)
    instant + (value - get(instant)) * iUnitMillis
  }

  def getRangeDurationField(): DurationField = iRangeField

  def getMaximumValue(): Int = iRange - 1

  def getRange(): Int = iRange
}
