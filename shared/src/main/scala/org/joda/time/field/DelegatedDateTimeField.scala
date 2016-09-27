package org.joda.time.field

import java.io.Serializable
import java.util.Locale
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial

@SerialVersionUID(-4730164440214502503L)
class DelegatedDateTimeField(private val field: DateTimeField,
                             private val rangeField: DurationField,
                             `type`: DateTimeFieldType)
    extends DateTimeField()
    with Serializable {

  private var iField: DateTimeField = null
  private var iRangeDurationField: DurationField = null

  if (field == null) {
    throw new IllegalArgumentException("The field must not be null")
  }

  iField = field
  iRangeDurationField = rangeField
  private val iType = if (`type` == null) field.getType else `type`

  def this(field: DateTimeField, `type`: DateTimeFieldType) {
    this(field, null, `type`)
  }

  def this(field: DateTimeField) {
    this(field, null)
  }

  def getWrappedField(): DateTimeField = iField

  def getType(): DateTimeFieldType = iType

  def getName(): String = iType.getName

  def isSupported(): Boolean = iField.isSupported

  def isLenient(): Boolean = iField.isLenient

  def get(instant: Long): Int = iField.get(instant)

  def getAsText(instant: Long, locale: Locale): String =
    iField.getAsText(instant, locale)

  def getAsText(instant: Long): String = iField.getAsText(instant)

  def getAsText(partial: ReadablePartial,
                fieldValue: Int,
                locale: Locale): String = {
    iField.getAsText(partial, fieldValue, locale)
  }

  def getAsText(partial: ReadablePartial, locale: Locale): String =
    iField.getAsText(partial, locale)

  def getAsText(fieldValue: Int, locale: Locale): String =
    iField.getAsText(fieldValue, locale)

  def getAsShortText(instant: Long, locale: Locale): String =
    iField.getAsShortText(instant, locale)

  def getAsShortText(instant: Long): String = iField.getAsShortText(instant)

  def getAsShortText(partial: ReadablePartial,
                     fieldValue: Int,
                     locale: Locale): String = {
    iField.getAsShortText(partial, fieldValue, locale)
  }

  def getAsShortText(partial: ReadablePartial, locale: Locale): String =
    iField.getAsShortText(partial, locale)

  def getAsShortText(fieldValue: Int, locale: Locale): String = {
    iField.getAsShortText(fieldValue, locale)
  }

  def add(instant: Long, value: Int): Long = iField.add(instant, value)

  def add(instant: Long, value: Long): Long = iField.add(instant, value)

  def add(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          valueToAdd: Int): Array[Int] = {
    iField.add(instant, fieldIndex, values, valueToAdd)
  }

  def addWrapPartial(instant: ReadablePartial,
                     fieldIndex: Int,
                     values: Array[Int],
                     valueToAdd: Int): Array[Int] = {
    iField.addWrapPartial(instant, fieldIndex, values, valueToAdd)
  }

  def addWrapField(instant: Long, value: Int): Long =
    iField.addWrapField(instant, value)

  def addWrapField(instant: ReadablePartial,
                   fieldIndex: Int,
                   values: Array[Int],
                   valueToAdd: Int): Array[Int] = {
    iField.addWrapField(instant, fieldIndex, values, valueToAdd)
  }

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    iField.getDifference(minuendInstant, subtrahendInstant)
  }

  def getDifferenceAsLong(minuendInstant: Long,
                          subtrahendInstant: Long): Long = {
    iField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  def set(instant: Long, value: Int): Long = iField.set(instant, value)

  def set(instant: Long, text: String, locale: Locale): Long =
    iField.set(instant, text, locale)

  def set(instant: Long, text: String): Long = iField.set(instant, text)

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          newValue: Int): Array[Int] = {
    iField.set(instant, fieldIndex, values, newValue)
  }

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          text: String,
          locale: Locale): Array[Int] = {
    iField.set(instant, fieldIndex, values, text, locale)
  }

  def getDurationField(): DurationField = iField.getDurationField

  def getRangeDurationField(): DurationField = {
    if (iRangeDurationField != null) {
      return iRangeDurationField
    }
    iField.getRangeDurationField
  }

  def isLeap(instant: Long): Boolean = iField.isLeap(instant)

  def getLeapAmount(instant: Long): Int = iField.getLeapAmount(instant)

  def getLeapDurationField(): DurationField = iField.getLeapDurationField

  def getMinimumValue(): Int = iField.getMinimumValue

  def getMinimumValue(instant: Long): Int = iField.getMinimumValue(instant)

  def getMinimumValue(instant: ReadablePartial): Int =
    iField.getMinimumValue(instant)

  def getMinimumValue(instant: ReadablePartial, values: Array[Int]): Int =
    iField.getMinimumValue(instant, values)

  def getMaximumValue(): Int = iField.getMaximumValue

  def getMaximumValue(instant: Long): Int = iField.getMaximumValue(instant)

  def getMaximumValue(instant: ReadablePartial): Int =
    iField.getMaximumValue(instant)

  def getMaximumValue(instant: ReadablePartial, values: Array[Int]): Int =
    iField.getMaximumValue(instant, values)

  def getMaximumTextLength(locale: Locale): Int =
    iField.getMaximumTextLength(locale)

  def getMaximumShortTextLength(locale: Locale): Int = {
    iField.getMaximumShortTextLength(locale)
  }

  def roundFloor(instant: Long): Long = iField.roundFloor(instant)

  def roundCeiling(instant: Long): Long = iField.roundCeiling(instant)

  def roundHalfFloor(instant: Long): Long = iField.roundHalfFloor(instant)

  def roundHalfCeiling(instant: Long): Long = iField.roundHalfCeiling(instant)

  def roundHalfEven(instant: Long): Long = iField.roundHalfEven(instant)

  def remainder(instant: Long): Long = iField.remainder(instant)

  override def toString(): String = "DateTimeField[" + getName + ']'
}
