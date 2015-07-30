package org.joda.time.chrono

import java.util.Locale
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import org.joda.time.field.BaseDateTimeField
import org.joda.time.field.FieldUtils
import org.joda.time.field.UnsupportedDurationField

@SerialVersionUID(4240986525305515528L)
class GJEraDateTimeField(private val iChronology: BasicChronology) extends BaseDateTimeField(DateTimeFieldType.era()) {

  def isLenient(): Boolean = false

  def get(instant: Long): Int = {
    if (iChronology.getYear(instant) <= 0) {
      DateTimeConstants.BCE
    } else {
      DateTimeConstants.CE
    }
  }

  override def getAsText(fieldValue: Int, locale: Locale): String = {
    GJLocaleSymbols.forLocale(locale).eraValueToText(fieldValue)
  }

  def set(instant: Long, era: Int): Long = {
    FieldUtils.verifyValueBounds(this, era, DateTimeConstants.BCE, DateTimeConstants.CE)
    val oldEra = get(instant)
    if (oldEra != era) {
      val year = iChronology.getYear(instant)
      iChronology.setYear(instant, -year)
    } else {
      instant
    }
  }

  override def set(instant: Long, text: String, locale: Locale): Long = {
    set(instant, GJLocaleSymbols.forLocale(locale).eraTextToValue(text))
  }

  def roundFloor(instant: Long): Long = {
    if (get(instant) == DateTimeConstants.CE) {
      iChronology.setYear(0, 1)
    } else {
      Long.MinValue
    }
  }

  override def roundCeiling(instant: Long): Long = {
    if (get(instant) == DateTimeConstants.BCE) {
      iChronology.setYear(0, 1)
    } else {
      Long.MaxValue
    }
  }

  override def roundHalfFloor(instant: Long): Long = roundFloor(instant)

  override def roundHalfCeiling(instant: Long): Long = roundFloor(instant)

  override def roundHalfEven(instant: Long): Long = roundFloor(instant)

  def getDurationField(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.eras())
  }

  def getRangeDurationField(): DurationField = null

  def getMinimumValue(): Int = DateTimeConstants.BCE

  def getMaximumValue(): Int = DateTimeConstants.CE

  override def getMaximumTextLength(locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).getEraMaxTextLength
  }

  private def readResolve(): AnyRef = iChronology.era()
}
