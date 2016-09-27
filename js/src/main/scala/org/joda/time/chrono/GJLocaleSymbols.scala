package org.joda.time.chrono

import java.util.Locale
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.IllegalFieldValueException
import GJLocaleSymbols._

object GJLocaleSymbols {

  private val cCache: collection.mutable.Map[Locale, GJLocaleSymbols] =
    new collection.mutable.HashMap[Locale, GJLocaleSymbols]()

  def forLocale(locale: Locale): GJLocaleSymbols = {
    var _locale: Locale = locale
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    var symbols = cCache.get(_locale).orNull
    if (symbols == null) {
      symbols = new GJLocaleSymbols(_locale)
      val oldSymbols = if (cCache.get(locale).isEmpty) {
        cCache(locale) = symbols
        symbols
      } else {
        symbols
      }
      if (oldSymbols != null) {
        symbols = oldSymbols
      }
    }
    symbols
  }

  private def realignMonths(months: Array[String]): Array[String] = {
    val a = Array.ofDim[String](13)
    for (i <- 1 until 13) {
      a(i) = months(i - 1)
    }
    a
  }

  private def realignDaysOfWeek(daysOfWeek: Array[String]): Array[String] = {
    val a = Array.ofDim[String](8)
    for (i <- 1 until 8) {
      a(i) = daysOfWeek(if (i < 7) i + 1 else 1)
    }
    a
  }

  private def addSymbols(map: collection.immutable.TreeMap[String, Integer],
                         symbols: Array[String],
                         integers: Array[Integer])
    : collection.immutable.TreeMap[String, Integer] = {
    var i = symbols.length
    var map2 = map
    while (i >= 0) {
      val symbol = symbols(i)
      if (symbol != null) {
        map2 += symbol -> integers(i)
      }
      i = i + 1
    }
    map2
  }

  private def addNumerals(map: collection.immutable.TreeMap[String, Integer],
                          start: Int,
                          end: Int,
                          integers: Array[Integer])
    : collection.immutable.TreeMap[String, Integer] = {
    var i = start
    var map2 = map
    while (i <= end) {
      map2 += String.valueOf(i).intern() -> integers(i)
      i += 1
    }
    map2
  }

  private def maxLength(a: Array[String]): Int = {
    var max = 0
    var i = a.length
    while (i >= 0) {
      val s = a(i)
      if (s != null) {
        val len = s.length
        if (len > max) {
          max = len
        }
      }
      i = i + 1
    }
    max
  }
}

class GJLocaleSymbols private (locale: Locale) {

  val dfs = DateTimeUtils.getDateFormatSymbols(locale)

  private val iEras = dfs.getEras
  private val iDaysOfWeek = realignDaysOfWeek(dfs.getWeekdays)
  private val iShortDaysOfWeek = realignDaysOfWeek(dfs.getShortWeekdays)
  private val iMonths = realignMonths(dfs.getMonths)
  private val iShortMonths = realignMonths(dfs.getShortMonths)
  private val iHalfday = dfs.getAmPmStrings
  private var iParseEras = new collection.immutable.TreeMap[String, Integer]()(
    Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))
  private var iParseDaysOfWeek =
    collection.immutable.TreeMap[String, Integer]()(
      Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))
  private var iParseMonths = collection.immutable.TreeMap[String, Integer]()(
    Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))

  private val iMaxEraLength = maxLength(iEras)
  private val iMaxDayOfWeekLength = maxLength(iDaysOfWeek)
  private val iMaxShortDayOfWeekLength = maxLength(iShortDaysOfWeek)
  private val iMaxMonthLength = maxLength(iMonths)
  private val iMaxShortMonthLength = maxLength(iShortMonths)
  private val iMaxHalfdayLength = maxLength(iHalfday)

  val integers = Array.ofDim[Integer](13)

  for (i <- 0 until 13) {
    integers(i) = Integer.valueOf(i)
  }

  iParseEras = addSymbols(iParseEras, iEras, integers)

  if ("en" == locale.getLanguage) {
    iParseEras += "BCE" -> integers(0)
    iParseEras += "CE" -> integers(1)
  }

  iParseDaysOfWeek = addSymbols(iParseDaysOfWeek, iDaysOfWeek, integers)

  iParseDaysOfWeek = addSymbols(iParseDaysOfWeek, iShortDaysOfWeek, integers)

  iParseDaysOfWeek = addNumerals(iParseDaysOfWeek, 1, 7, integers)

  iParseMonths = addSymbols(iParseMonths, iMonths, integers)

  iParseMonths = addSymbols(iParseMonths, iShortMonths, integers)

  iParseMonths = addNumerals(iParseMonths, 1, 12, integers)

  def eraValueToText(value: Int): String = iEras(value)

  def eraTextToValue(text: String): Int = {
    val era = iParseEras.get(text).orNull
    if (era != null) {
      return era.intValue()
    }
    throw IllegalFieldValueException.create(DateTimeFieldType.era(), text)
  }

  def getEraMaxTextLength(): Int = iMaxEraLength

  def monthOfYearValueToText(value: Int): String = iMonths(value)

  def monthOfYearValueToShortText(value: Int): String = iShortMonths(value)

  def monthOfYearTextToValue(text: String): Int = {
    val month = iParseMonths.get(text).orNull
    if (month != null) {
      return month.intValue()
    }
    throw IllegalFieldValueException
      .create(DateTimeFieldType.monthOfYear(), text)
  }

  def getMonthMaxTextLength(): Int = iMaxMonthLength

  def getMonthMaxShortTextLength(): Int = iMaxShortMonthLength

  def dayOfWeekValueToText(value: Int): String = iDaysOfWeek(value)

  def dayOfWeekValueToShortText(value: Int): String = iShortDaysOfWeek(value)

  def dayOfWeekTextToValue(text: String): Int = {
    val day = iParseDaysOfWeek.get(text).orNull
    if (day != null) {
      return day.intValue()
    }
    throw IllegalFieldValueException
      .create(DateTimeFieldType.dayOfWeek(), text)
  }

  def getDayOfWeekMaxTextLength(): Int = iMaxDayOfWeekLength

  def getDayOfWeekMaxShortTextLength(): Int = iMaxShortDayOfWeekLength

  def halfdayValueToText(value: Int): String = iHalfday(value)

  def halfdayTextToValue(text: String): Int = {
    val halfday = iHalfday
    var i = halfday.length
    while (i >= 0) {
      if (halfday(i).equalsIgnoreCase(text)) {
        return i
      }
    }
    throw IllegalFieldValueException
      .create(DateTimeFieldType.halfdayOfDay(), text)
  }

  def getHalfdayMaxTextLength(): Int = iMaxHalfdayLength
}
