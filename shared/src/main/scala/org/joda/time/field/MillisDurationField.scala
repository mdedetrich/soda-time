package org.joda.time.field

import java.io.Serializable
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import MillisDurationField._

object MillisDurationField {

  val INSTANCE = new MillisDurationField()
}

@SerialVersionUID(2656707858124633367L)
class MillisDurationField private ()
    extends DurationField()
    with Serializable {

  def getType(): DurationFieldType = DurationFieldType.millis()

  def getName(): String = "millis"

  def isSupported(): Boolean = true

  def isPrecise(): Boolean = true

  def getUnitMillis(): Long = 1

  def getValue(duration: Long): Int = FieldUtils.safeToInt(duration)

  def getValueAsLong(duration: Long): Long = duration

  def getValue(duration: Long, instant: Long): Int =
    FieldUtils.safeToInt(duration)

  def getValueAsLong(duration: Long, instant: Long): Long = duration

  def getMillis(value: Int): Long = value

  def getMillis(value: Long): Long = value

  def getMillis(value: Int, instant: Long): Long = value

  def getMillis(value: Long, instant: Long): Long = value

  def add(instant: Long, value: Int): Long = FieldUtils.safeAdd(instant, value)

  def add(instant: Long, value: Long): Long =
    FieldUtils.safeAdd(instant, value)

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    FieldUtils.safeToInt(
      FieldUtils.safeSubtract(minuendInstant, subtrahendInstant))
  }

  def getDifferenceAsLong(minuendInstant: Long,
                          subtrahendInstant: Long): Long = {
    FieldUtils.safeSubtract(minuendInstant, subtrahendInstant)
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

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[MillisDurationField]) {
      return getUnitMillis ==
        obj.asInstanceOf[MillisDurationField].getUnitMillis
    }
    false
  }

  override def hashCode(): Int = getUnitMillis.toInt

  override def toString(): String = "DurationField[millis]"

  private def readResolve(): AnyRef = INSTANCE
}
