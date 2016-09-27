package org.joda.time.field

import org.joda.time.DurationFieldType

@SerialVersionUID(-8346152187724495365L)
class PreciseDurationField(`type`: DurationFieldType,
                           private val iUnitMillis: Long)
    extends BaseDurationField(`type`) {

  def isPrecise(): Boolean = true

  def getUnitMillis(): Long = iUnitMillis

  def getValueAsLong(duration: Long, instant: Long): Long =
    duration / iUnitMillis

  def getMillis(value: Int, instant: Long): Long = value * iUnitMillis

  def getMillis(value: Long, instant: Long): Long = {
    FieldUtils.safeMultiply(value, iUnitMillis)
  }

  def add(instant: Long, value: Int): Long = {
    val addition = value * iUnitMillis
    FieldUtils.safeAdd(instant, addition)
  }

  def add(instant: Long, value: Long): Long = {
    val addition = FieldUtils.safeMultiply(value, iUnitMillis)
    FieldUtils.safeAdd(instant, addition)
  }

  def getDifferenceAsLong(minuendInstant: Long,
                          subtrahendInstant: Long): Long = {
    val difference = FieldUtils.safeSubtract(minuendInstant, subtrahendInstant)
    difference / iUnitMillis
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    } else if (obj.isInstanceOf[PreciseDurationField]) {
      val other = obj.asInstanceOf[PreciseDurationField]
      return (getType == other.getType) && (iUnitMillis == other.iUnitMillis)
    }
    false
  }

  override def hashCode(): Int = {
    val millis = iUnitMillis
    var hash = (millis ^ (millis >>> 32)).toInt
    hash += getType.hashCode
    hash
  }
}
