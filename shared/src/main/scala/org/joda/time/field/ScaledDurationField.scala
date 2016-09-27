package org.joda.time.field

import org.joda.time.DurationField
import org.joda.time.DurationFieldType

@SerialVersionUID(-3205227092378684157L)
class ScaledDurationField(field: DurationField,
                          `type`: DurationFieldType,
                          private val scalar: Int)
    extends DecoratedDurationField(field, `type`) {

  private var iScalar: Int = _

  if (scalar == 0 || scalar == 1) {
    throw new IllegalArgumentException("The scalar must not be 0 or 1")
  }

  iScalar = scalar

  override def getValue(duration: Long): Int = {
    getWrappedField.getValue(duration) / iScalar
  }

  override def getValueAsLong(duration: Long): Long = {
    getWrappedField.getValueAsLong(duration) / iScalar
  }

  override def getValue(duration: Long, instant: Long): Int = {
    getWrappedField.getValue(duration, instant) / iScalar
  }

  override def getValueAsLong(duration: Long, instant: Long): Long = {
    getWrappedField.getValueAsLong(duration, instant) / iScalar
  }

  override def getMillis(value: Int): Long = {
    val scaled = value.toLong * iScalar.toLong
    getWrappedField.getMillis(scaled)
  }

  override def getMillis(value: Long): Long = {
    val scaled = FieldUtils.safeMultiply(value, iScalar)
    getWrappedField.getMillis(scaled)
  }

  override def getMillis(value: Int, instant: Long): Long = {
    val scaled = value.toLong * iScalar.toLong
    getWrappedField.getMillis(scaled, instant)
  }

  override def getMillis(value: Long, instant: Long): Long = {
    val scaled = FieldUtils.safeMultiply(value, iScalar)
    getWrappedField.getMillis(scaled, instant)
  }

  override def add(instant: Long, value: Int): Long = {
    val scaled = value.toLong * iScalar.toLong
    getWrappedField.add(instant, scaled)
  }

  override def add(instant: Long, value: Long): Long = {
    val scaled = FieldUtils.safeMultiply(value, iScalar)
    getWrappedField.add(instant, scaled)
  }

  override def getDifference(minuendInstant: Long,
                             subtrahendInstant: Long): Int = {
    getWrappedField.getDifference(minuendInstant, subtrahendInstant) /
      iScalar
  }

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant) /
      iScalar
  }

  override def getUnitMillis(): Long = getWrappedField.getUnitMillis * iScalar

  def getScalar(): Int = iScalar

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    } else if (obj.isInstanceOf[ScaledDurationField]) {
      val other = obj.asInstanceOf[ScaledDurationField]
      return (getWrappedField == other.getWrappedField) && (getType == other.getType) &&
        (iScalar == other.iScalar)
    }
    false
  }

  override def hashCode(): Int = {
    val scalar = iScalar
    var hash = (scalar ^ (scalar >>> 32)).toInt
    hash += getType.hashCode
    hash += getWrappedField.hashCode
    hash
  }
}
