package org.joda.time.field

import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial

@SerialVersionUID(961749798233026866L)
class ZeroIsMaxDateTimeField(field: DateTimeField, `type`: DateTimeFieldType)
    extends DecoratedDateTimeField(field, `type`) {

  if (field.getMinimumValue != 0) {
    throw new IllegalArgumentException(
      "Wrapped field's minumum value must be zero")
  }

  override def get(instant: Long): Int = {
    var value = getWrappedField.get(instant)
    if (value == 0) {
      value = getMaximumValue
    }
    value
  }

  override def add(instant: Long, value: Int): Long =
    getWrappedField.add(instant, value)

  override def add(instant: Long, value: Long): Long =
    getWrappedField.add(instant, value)

  override def addWrapField(instant: Long, value: Int): Long = {
    getWrappedField.addWrapField(instant, value)
  }

  override def addWrapField(instant: ReadablePartial,
                            fieldIndex: Int,
                            values: Array[Int],
                            valueToAdd: Int): Array[Int] = {
    getWrappedField.addWrapField(instant, fieldIndex, values, valueToAdd)
  }

  override def getDifference(minuendInstant: Long,
                             subtrahendInstant: Long): Int = {
    getWrappedField.getDifference(minuendInstant, subtrahendInstant)
  }

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  override def set(instant: Long, value: Int): Long = {
    var _value: Int = value
    val max = getMaximumValue
    FieldUtils.verifyValueBounds(this, _value, 1, max)
    if (_value == max) {
      _value = 0
    }
    getWrappedField.set(instant, _value)
  }

  override def isLeap(instant: Long): Boolean = getWrappedField.isLeap(instant)

  override def getLeapAmount(instant: Long): Int =
    getWrappedField.getLeapAmount(instant)

  override def getLeapDurationField(): DurationField =
    getWrappedField.getLeapDurationField

  override def getMinimumValue(): Int = 1

  override def getMinimumValue(instant: Long): Int = 1

  override def getMinimumValue(instant: ReadablePartial): Int = 1

  override def getMinimumValue(instant: ReadablePartial,
                               values: Array[Int]): Int = 1

  override def getMaximumValue(): Int = getWrappedField.getMaximumValue + 1

  override def getMaximumValue(instant: Long): Int = {
    getWrappedField.getMaximumValue(instant) + 1
  }

  override def getMaximumValue(instant: ReadablePartial): Int = {
    getWrappedField.getMaximumValue(instant) + 1
  }

  override def getMaximumValue(instant: ReadablePartial,
                               values: Array[Int]): Int = {
    getWrappedField.getMaximumValue(instant, values) + 1
  }

  override def roundFloor(instant: Long): Long =
    getWrappedField.roundFloor(instant)

  override def roundCeiling(instant: Long): Long =
    getWrappedField.roundCeiling(instant)

  override def roundHalfFloor(instant: Long): Long =
    getWrappedField.roundHalfFloor(instant)

  override def roundHalfCeiling(instant: Long): Long = {
    getWrappedField.roundHalfCeiling(instant)
  }

  override def roundHalfEven(instant: Long): Long =
    getWrappedField.roundHalfEven(instant)

  override def remainder(instant: Long): Long =
    getWrappedField.remainder(instant)
}
