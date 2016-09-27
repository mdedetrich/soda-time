package org.joda.time.field

import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField

@SerialVersionUID(203115783733757597L)
abstract class DecoratedDateTimeField protected (
    private val field: DateTimeField,
    `type`: DateTimeFieldType)
    extends BaseDateTimeField(`type`) {

  private var iField: DateTimeField = null

  if (field == null) {
    throw new IllegalArgumentException("The field must not be null")
  }

  if (!field.isSupported) {
    throw new IllegalArgumentException("The field must be supported")
  }

  iField = field

  def getWrappedField(): DateTimeField = iField

  def isLenient(): Boolean = iField.isLenient

  def get(instant: Long): Int = iField.get(instant)

  def set(instant: Long, value: Int): Long = iField.set(instant, value)

  def getDurationField(): DurationField = iField.getDurationField

  def getRangeDurationField(): DurationField = iField.getRangeDurationField

  def getMinimumValue(): Int = iField.getMinimumValue

  def getMaximumValue(): Int = iField.getMaximumValue

  def roundFloor(instant: Long): Long = iField.roundFloor(instant)
}
