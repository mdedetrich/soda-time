package org.joda.time.chrono

import java.util.Locale

@SerialVersionUID(-4748157875845286249L)
class GJMonthOfYearDateTimeField(chronology: BasicChronology)
    extends BasicMonthOfYearDateTimeField(chronology, 2) {

  override def getAsText(fieldValue: Int, locale: Locale): String = {
    GJLocaleSymbols.forLocale(locale).monthOfYearValueToText(fieldValue)
  }

  override def getAsShortText(fieldValue: Int, locale: Locale): String = {
    GJLocaleSymbols.forLocale(locale).monthOfYearValueToShortText(fieldValue)
  }

  override protected def convertText(text: String, locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).monthOfYearTextToValue(text)
  }

  override def getMaximumTextLength(locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).getMonthMaxTextLength
  }

  override def getMaximumShortTextLength(locale: Locale): Int = {
    GJLocaleSymbols.forLocale(locale).getMonthMaxShortTextLength
  }
}
