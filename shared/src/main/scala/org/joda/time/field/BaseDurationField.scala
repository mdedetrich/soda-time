package org.joda.time.field

import java.io.Serializable
import org.joda.time.DurationField
import org.joda.time.DurationFieldType

@SerialVersionUID(-2554245107589433218L)
abstract class BaseDurationField protected (
    private val `type`: DurationFieldType)
    extends DurationField()
    with Serializable {

  private var iType: DurationFieldType = null

  if (`type` == null) {
    throw new IllegalArgumentException("The type must not be null")
  }

  iType = `type`

  def getType(): DurationFieldType = iType

  def getName(): String = iType.getName

  def isSupported(): Boolean = true

  def getValue(duration: Long): Int = {
    FieldUtils.safeToInt(getValueAsLong(duration))
  }

  def getValueAsLong(duration: Long): Long = duration / getUnitMillis

  def getValue(duration: Long, instant: Long): Int = {
    FieldUtils.safeToInt(getValueAsLong(duration, instant))
  }

  def getMillis(value: Int): Long = value * getUnitMillis

  def getMillis(value: Long): Long = {
    FieldUtils.safeMultiply(value, getUnitMillis)
  }

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    FieldUtils.safeToInt(
      getDifferenceAsLong(minuendInstant, subtrahendInstant))
  }

  def compareTo(otherField: DurationField): Int = {
    val otherMillis = otherField.getUnitMillis
    val thisMillis = getUnitMillis
    if (thisMillis == otherMillis) {
      return 0
    }
    if (thisMillis < otherMillis) {
      -1
    } else {
      1
    }
  }

  override def toString(): String = "DurationField[" + getName + ']'
}
