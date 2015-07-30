package org.joda.time.field

import java.io.Serializable
import java.util.Locale
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import UnsupportedDateTimeField._

object UnsupportedDateTimeField {

  private var cCache: collection.mutable.HashMap[DateTimeFieldType, UnsupportedDateTimeField] = _

  def getInstance(`type`: DateTimeFieldType, durationField: DurationField): UnsupportedDateTimeField = {
    synchronized {
      var field: UnsupportedDateTimeField = null
      if (cCache == null) {
        cCache = new collection.mutable.HashMap[DateTimeFieldType, UnsupportedDateTimeField]()
        field = null
      } else {
        field = cCache.get(`type`).orNull
        if (field != null && field.getDurationField != durationField) {
          field = null
        }
      }
      if (field == null) {
        field = new UnsupportedDateTimeField(`type`, durationField)
        cCache.put(`type`, field)
      }
      field
    }
  }
}

@SerialVersionUID(-1934618396111902255L)
class UnsupportedDateTimeField private (private val `type`: DateTimeFieldType, private val durationField: DurationField)
  extends DateTimeField with Serializable {

  private var iType: DateTimeFieldType = null
  private var iDurationField: DurationField = null

  if (`type` == null || durationField == null) {
    throw new IllegalArgumentException()
  }
  iType = `type`
  iDurationField = durationField

  def getType(): DateTimeFieldType = iType

  def getName(): String = iType.getName

  def isSupported(): Boolean = false

  def isLenient(): Boolean = false

  def get(instant: Long): Int = throw unsupported()

  def getAsText(instant: Long, locale: Locale): String = throw unsupported()

  def getAsText(instant: Long): String = throw unsupported()

  def getAsText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String = throw unsupported()

  def getAsText(partial: ReadablePartial, locale: Locale): String = throw unsupported()

  def getAsText(fieldValue: Int, locale: Locale): String = throw unsupported()

  def getAsShortText(instant: Long, locale: Locale): String = throw unsupported()

  def getAsShortText(instant: Long): String = throw unsupported()

  def getAsShortText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String = throw unsupported()

  def getAsShortText(partial: ReadablePartial, locale: Locale): String = throw unsupported()

  def getAsShortText(fieldValue: Int, locale: Locale): String = throw unsupported()

  def add(instant: Long, value: Int): Long = getDurationField.add(instant, value)

  def add(instant: Long, value: Long): Long = getDurationField.add(instant, value)

  def add(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          valueToAdd: Int): Array[Int] = throw unsupported()

  def addWrapPartial(instant: ReadablePartial,
                     fieldIndex: Int,
                     values: Array[Int],
                     valueToAdd: Int): Array[Int] = throw unsupported()

  def addWrapField(instant: Long, value: Int): Long = throw unsupported()

  def addWrapField(instant: ReadablePartial,
                   fieldIndex: Int,
                   values: Array[Int],
                   valueToAdd: Int): Array[Int] = throw unsupported()

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    getDurationField.getDifference(minuendInstant, subtrahendInstant)
  }

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
    getDurationField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  def set(instant: Long, value: Int): Long = throw unsupported()

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          newValue: Int): Array[Int] = throw unsupported()

  def set(instant: Long, text: String, locale: Locale): Long = throw unsupported()

  def set(instant: Long, text: String): Long = throw unsupported()

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          text: String,
          locale: Locale): Array[Int] = throw unsupported()

  def getDurationField(): DurationField = iDurationField

  def getRangeDurationField(): DurationField = null

  def isLeap(instant: Long): Boolean = throw unsupported()

  def getLeapAmount(instant: Long): Int = throw unsupported()

  def getLeapDurationField(): DurationField = null

  def getMinimumValue(): Int = throw unsupported()

  def getMinimumValue(instant: Long): Int = throw unsupported()

  def getMinimumValue(instant: ReadablePartial): Int = throw unsupported()

  def getMinimumValue(instant: ReadablePartial, values: Array[Int]): Int = throw unsupported()

  def getMaximumValue(): Int = throw unsupported()

  def getMaximumValue(instant: Long): Int = throw unsupported()

  def getMaximumValue(instant: ReadablePartial): Int = throw unsupported()

  def getMaximumValue(instant: ReadablePartial, values: Array[Int]): Int = throw unsupported()

  def getMaximumTextLength(locale: Locale): Int = throw unsupported()

  def getMaximumShortTextLength(locale: Locale): Int = throw unsupported()

  def roundFloor(instant: Long): Long = throw unsupported()

  def roundCeiling(instant: Long): Long = throw unsupported()

  def roundHalfFloor(instant: Long): Long = throw unsupported()

  def roundHalfCeiling(instant: Long): Long = throw unsupported()

  def roundHalfEven(instant: Long): Long = throw unsupported()

  def remainder(instant: Long): Long = throw unsupported()

  override def toString(): String = "UnsupportedDateTimeField"

  private def readResolve(): AnyRef = getInstance(iType, iDurationField)

  private def unsupported(): UnsupportedOperationException = {
    new UnsupportedOperationException(iType + " field is unsupported")
  }
}
