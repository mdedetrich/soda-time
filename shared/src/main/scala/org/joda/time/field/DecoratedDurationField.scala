package org.joda.time.field

import org.joda.time.DurationField
import org.joda.time.DurationFieldType

@SerialVersionUID(8019982251647420015L)
class DecoratedDurationField(private val field: DurationField, `type`: DurationFieldType)
  extends BaseDurationField(`type`) {
  
  private var iField: DurationField = null

  if (field == null) {
    throw new IllegalArgumentException("The field must not be null")
  }

  if (!field.isSupported) {
    throw new IllegalArgumentException("The field must be supported")
  }
  
  iField = field

  def getWrappedField(): DurationField = iField

  def isPrecise(): Boolean = iField.isPrecise

  def getValueAsLong(duration: Long, instant: Long): Long = {
    iField.getValueAsLong(duration, instant)
  }

  def getMillis(value: Int, instant: Long): Long = iField.getMillis(value, instant)

  def getMillis(value: Long, instant: Long): Long = iField.getMillis(value, instant)

  def add(instant: Long, value: Int): Long = iField.add(instant, value)

  def add(instant: Long, value: Long): Long = iField.add(instant, value)

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
    iField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  def getUnitMillis(): Long = iField.getUnitMillis
}
