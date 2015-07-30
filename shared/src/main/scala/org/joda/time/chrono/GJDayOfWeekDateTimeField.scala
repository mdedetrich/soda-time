package org.joda.time.chrono

import java.util.Locale
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.field.PreciseDurationDateTimeField

@SerialVersionUID(-3857947176719041436L)
class GJDayOfWeekDateTimeField(private val iChronology: BasicChronology, days: DurationField)
  extends PreciseDurationDateTimeField(DateTimeFieldType.dayOfWeek(), days) {

  def get(instant: Long): Int = iChronology.getDayOfWeek(instant)

  override def getAsText(fieldValue: Int, locale: Locale): String = {
    GJLocaleSymbols.forLocale(locale).dayOfWeekValueToText(fieldValue)
  }

  override def getAsShortText(fieldValue: Int, locale: Locale): String = {
    GJLocaleSymbols.forLocale(locale).dayOfWeekValueToShortText(fieldValue)
  }

  override protected def convertText(text: String, locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).dayOfWeekTextToValue(text)
  }

  def getRangeDurationField(): DurationField = iChronology.weeks()

  override def getMinimumValue(): Int = DateTimeConstants.MONDAY

  def getMaximumValue(): Int = DateTimeConstants.SUNDAY

  override def getMaximumTextLength(locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).getDayOfWeekMaxTextLength
  }

  override def getMaximumShortTextLength(locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).getDayOfWeekMaxShortTextLength
  }

  private def readResolve(): AnyRef = iChronology.dayOfWeek()
}
