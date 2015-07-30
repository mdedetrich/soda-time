package org.joda.time.field

import org.joda.time.{DateTimeField, DateTimeFieldType, DurationField}

@SerialVersionUID(3145790132623583142L)
class OffsetDateTimeField(field: DateTimeField,
                          `type`: DateTimeFieldType,
                          private val offset: Int,
                          minValue: Int,
                          maxValue: Int) extends DecoratedDateTimeField(field, `type`) {

  private var iOffset: Int = _
  private val iMin = if (minValue < (field.getMinimumValue + offset)) field.getMinimumValue + offset else minValue
  private val iMax = if (maxValue > (field.getMaximumValue + offset)) field.getMaximumValue + offset else maxValue
  
  iOffset = offset  

  if (offset == 0) {
    throw new IllegalArgumentException("The offset cannot be zero")
  }

  def this(field: DateTimeField, offset: Int) {
    this(field, if (field == null) null else field.getType, offset, Integer.MIN_VALUE, Integer.MAX_VALUE)
  }

  def this(field: DateTimeField, `type`: DateTimeFieldType, offset: Int) {
    this(field, `type`, offset, Integer.MIN_VALUE, Integer.MAX_VALUE)
  }

  override def get(instant: Long): Int = super.get(instant) + iOffset

  override def add(instant: Long, amount: Int): Long = {
    var _instant: Long = instant
    _instant = super.add(_instant, amount)
    FieldUtils.verifyValueBounds(this, get(_instant), iMin, iMax)
    _instant
  }

  override def add(instant: Long, amount: Long): Long = {
    var _instant: Long = instant
    _instant = super.add(_instant, amount)
    FieldUtils.verifyValueBounds(this, get(_instant), iMin, iMax)
    _instant
  }

  override def addWrapField(instant: Long, amount: Int): Long = {
    set(instant, FieldUtils.getWrappedValue(get(instant), amount, iMin, iMax))
  }

  override def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, iMin, iMax)
    super.set(instant, value - iOffset)
  }

  override def isLeap(instant: Long): Boolean = getWrappedField.isLeap(instant)

  override def getLeapAmount(instant: Long): Int = getWrappedField.getLeapAmount(instant)

  override def getLeapDurationField(): DurationField = getWrappedField.getLeapDurationField

  override def getMinimumValue(): Int = iMin

  override def getMaximumValue(): Int = iMax

  override def roundFloor(instant: Long): Long = getWrappedField.roundFloor(instant)

  override def roundCeiling(instant: Long): Long = getWrappedField.roundCeiling(instant)

  override def roundHalfFloor(instant: Long): Long = getWrappedField.roundHalfFloor(instant)

  override def roundHalfCeiling(instant: Long): Long = {
    getWrappedField.roundHalfCeiling(instant)
  }

  override def roundHalfEven(instant: Long): Long = getWrappedField.roundHalfEven(instant)

  override def remainder(instant: Long): Long = getWrappedField.remainder(instant)

  def getOffset(): Int = iOffset
}
