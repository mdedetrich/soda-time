package org.joda.time.field

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField

@SerialVersionUID(5004523158306266035L)
abstract class PreciseDurationDateTimeField(`type`: DateTimeFieldType, private val unit: DurationField)
  extends BaseDateTimeField(`type`) {

  val iUnitMillis = unit.getUnitMillis
  
  var iUnitField: DurationField = null

  if (!unit.isPrecise) {
    throw new IllegalArgumentException("Unit duration field must be precise")
  }

  if (iUnitMillis < 1) {
    throw new IllegalArgumentException("The unit milliseconds must be at least 1")
  }
  
  iUnitField = unit

  def isLenient(): Boolean = false

  def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, getMinimumValue, getMaximumValueForSet(instant, value))
    instant + (value - get(instant)) * iUnitMillis
  }

  def roundFloor(instant: Long): Long = {
    var _instant:Long = instant
    if (_instant >= 0) {
      _instant - _instant % iUnitMillis
    } else {
      _instant += 1
      _instant - _instant % iUnitMillis - iUnitMillis
    }
  }

  override def roundCeiling(instant: Long): Long = {
    var _instant:Long = instant
    if (_instant > 0) {
      _instant -= 1
      _instant - _instant % iUnitMillis + iUnitMillis
    } else {
      _instant - _instant % iUnitMillis
    }
  }

  override def remainder(instant: Long): Long = {
    if (instant >= 0) {
      instant % iUnitMillis
    } else {
      (instant + 1) % iUnitMillis + iUnitMillis - 1
    }
  }

  def getDurationField(): DurationField = iUnitField

  def getMinimumValue(): Int = 0

  def getUnitMillis(): Long = iUnitMillis

  protected def getMaximumValueForSet(instant: Long, value: Int): Int = getMaximumValue(instant)
}
