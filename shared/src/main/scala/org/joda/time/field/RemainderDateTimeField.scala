package org.joda.time.field

import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField

@SerialVersionUID(5708241235177666790L)
class RemainderDateTimeField(field: DateTimeField, `type`: DateTimeFieldType, val divisor: Int)
  extends DecoratedDateTimeField(field, `type`) {

  var iDurationField = field.getDurationField
  var iDivisor:Int = _
  
  val rangeField = field.getDurationField()

  var iRangeField:DurationField = if (rangeField == null) null else new ScaledDurationField(rangeField, `type`.getRangeDurationType,
    divisor)

  if (divisor < 2) {
    throw new IllegalArgumentException("The divisor must be at least 2")
  }
  

  iDurationField = field.getDurationField
  iDivisor = divisor


  def this(dividedField: DividedDateTimeField, durationField: DurationField, `type`: DateTimeFieldType) {
    this(dividedField.getWrappedField,`type`,dividedField.iDivisor)
  }

  def this(dividedField: DividedDateTimeField, `type`: DateTimeFieldType) {
    this(dividedField, dividedField.getWrappedField.getDurationField, `type`)
  }

  def this(dividedField: DividedDateTimeField) {
    this(dividedField, dividedField.getType)
  }

  def this(field: DateTimeField,
           rangeField: DurationField,
           `type`: DateTimeFieldType,
           divisor: Int) {
    this(field,`type`,divisor)
  }

  override def get(instant: Long): Int = {
    val value = getWrappedField.get(instant)
    if (value >= 0) {
      value % iDivisor
    } else {
      (iDivisor - 1) + ((value + 1) % iDivisor)
    }
  }

  override def addWrapField(instant: Long, amount: Int): Long = {
    set(instant, FieldUtils.getWrappedValue(get(instant), amount, 0, iDivisor - 1))
  }

  override def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, 0, iDivisor - 1)
    val divided = getDivided(getWrappedField.get(instant))
    getWrappedField.set(instant, divided * iDivisor + value)
  }

  override def getDurationField(): DurationField = iDurationField

  override def getRangeDurationField(): DurationField = iRangeField

  override def getMinimumValue(): Int = 0

  override def getMaximumValue(): Int = iDivisor - 1

  override def roundFloor(instant: Long): Long = getWrappedField.roundFloor(instant)

  override def roundCeiling(instant: Long): Long = getWrappedField.roundCeiling(instant)

  override def roundHalfFloor(instant: Long): Long = getWrappedField.roundHalfFloor(instant)

  override def roundHalfCeiling(instant: Long): Long = {
    getWrappedField.roundHalfCeiling(instant)
  }

  override def roundHalfEven(instant: Long): Long = getWrappedField.roundHalfEven(instant)

  override def remainder(instant: Long): Long = getWrappedField.remainder(instant)

  def getDivisor(): Int = iDivisor

  private def getDivided(value: Int): Int = {
    if (value >= 0) {
      value / iDivisor
    } else {
      ((value + 1) / iDivisor) - 1
    }
  }
}
