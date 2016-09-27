package org.joda.time.field

import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField

@SerialVersionUID(8318475124230605365L)
class DividedDateTimeField(field: DateTimeField,
                           val rangeField: DurationField,
                           `type`: DateTimeFieldType,
                           val divisor: Int)
    extends DecoratedDateTimeField(field, `type`) {

  var iDivisor: Int = _
  var iRangeDurationField: DurationField = null
  private var iMin: Int = _
  private var iMax: Int = _

  if (divisor < 2) {
    throw new IllegalArgumentException("The divisor must be at least 2")
  }

  iRangeDurationField = rangeField
  iDivisor = divisor

  val unitField = field.getDurationField

  var iDurationField: DurationField =
    if (unitField == null) null
    else new ScaledDurationField(unitField, `type`.getDurationType, divisor)

  val i = field.getMinimumValue
  val min = if (i >= 0) i / divisor else (i + 1) / divisor - 1
  val j = field.getMaximumValue
  val max = if (j >= 0) j / divisor else (j + 1) / divisor - 1

  def this(field: DateTimeField, `type`: DateTimeFieldType, divisor: Int) {
    this(field, field.getRangeDurationField, `type`, divisor)
  }

  def this(remainderField: RemainderDateTimeField,
           rangeField: DurationField,
           `type`: DateTimeFieldType) {
    this(remainderField, `type`, remainderField.iDivisor)
    iDivisor = remainderField.iDivisor
    val divisor = iDivisor
    iDurationField = remainderField.iRangeField
    iRangeDurationField = rangeField
    val field = getWrappedField
    val i = field.getMinimumValue
    val min = if (i >= 0) i / divisor else (i + 1) / divisor - 1
    val j = field.getMaximumValue
    val max = if (j >= 0) j / divisor else (j + 1) / divisor - 1
    iMin = min
    iMax = max
  }

  def this(remainderField: RemainderDateTimeField, `type`: DateTimeFieldType) {
    this(remainderField, null, `type`)
  }

  override def getRangeDurationField(): DurationField = {
    if (iRangeDurationField != null) {
      return iRangeDurationField
    }
    super.getRangeDurationField
  }

  override def get(instant: Long): Int = {
    val value = getWrappedField.get(instant)
    if (value >= 0) {
      value / iDivisor
    } else {
      ((value + 1) / iDivisor) - 1
    }
  }

  override def add(instant: Long, amount: Int): Long = {
    getWrappedField.add(instant, amount * iDivisor)
  }

  override def add(instant: Long, amount: Long): Long = {
    getWrappedField.add(instant, amount * iDivisor)
  }

  override def addWrapField(instant: Long, amount: Int): Long = {
    set(instant, FieldUtils.getWrappedValue(get(instant), amount, iMin, iMax))
  }

  override def getDifference(minuendInstant: Long,
                             subtrahendInstant: Long): Int = {
    getWrappedField.getDifference(minuendInstant, subtrahendInstant) /
      iDivisor
  }

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant) /
      iDivisor
  }

  override def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, iMin, iMax)
    val remainder = getRemainder(getWrappedField.get(instant))
    getWrappedField.set(instant, value * iDivisor + remainder)
  }

  override def getDurationField(): DurationField = iDurationField

  override def getMinimumValue(): Int = iMin

  override def getMaximumValue(): Int = iMax

  override def roundFloor(instant: Long): Long = {
    val field = getWrappedField
    field.roundFloor(field.set(instant, get(instant) * iDivisor))
  }

  override def remainder(instant: Long): Long = {
    set(instant, get(getWrappedField.remainder(instant)))
  }

  def getDivisor(): Int = iDivisor

  private def getRemainder(value: Int): Int = {
    if (value >= 0) {
      value % iDivisor
    } else {
      (iDivisor - 1) + ((value + 1) % iDivisor)
    }
  }
}
