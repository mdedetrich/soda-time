package org.joda.time.chrono

import java.util.Locale
import java.util.TreeMap
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.IllegalFieldValueException
import GJLocaleSymbols._

object GJLocaleSymbols {

  private val cCache: ConcurrentMap[Locale, GJLocaleSymbols] = new ConcurrentHashMap[Locale, GJLocaleSymbols]()

  def forLocale(locale: Locale): GJLocaleSymbols = {
    var _locale: Locale = locale
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    var symbols = cCache.get(_locale)
    if (symbols == null) {
      symbols = new GJLocaleSymbols(_locale)
      val oldSymbols = cCache.putIfAbsent(_locale, symbols)
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
      a(i) = daysOfWeek(if ((i < 7)) i + 1 else 1)
    }
    a
  }

  private def addSymbols(map: TreeMap[String, Integer], symbols: Array[String], integers: Array[Integer]) {
    var i = symbols.length
    while (i >= 0) {
      val symbol = symbols(i)
      if (symbol != null) {
        map.put(symbol, integers(i))
      }
    }
  }

  private def addNumerals(map: TreeMap[String, Integer],
                          start: Int,
                          end: Int,
                          integers: Array[Integer]) {
    var i = start
    while (i <= end) {
      map.put(String.valueOf(i).intern(), integers(i))
      i += 1
    }
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
  private val iParseEras = new TreeMap[String, Integer](String.CASE_INSENSITIVE_ORDER)
  private val iParseDaysOfWeek = new TreeMap[String, Integer](String.CASE_INSENSITIVE_ORDER)
  private val iParseMonths = new TreeMap[String, Integer](String.CASE_INSENSITIVE_ORDER)
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

  addSymbols(iParseEras, iEras, integers)

  if ("en" == locale.getLanguage) {
    iParseEras.put("BCE", integers(0))
    iParseEras.put("CE", integers(1))
  }

  addSymbols(iParseDaysOfWeek, iDaysOfWeek, integers)

  addSymbols(iParseDaysOfWeek, iShortDaysOfWeek, integers)

  addNumerals(iParseDaysOfWeek, 1, 7, integers)

  addSymbols(iParseMonths, iMonths, integers)

  addSymbols(iParseMonths, iShortMonths, integers)

  addNumerals(iParseMonths, 1, 12, integers)

  def eraValueToText(value: Int): String = iEras(value)

  def eraTextToValue(text: String): Int = {
    val era = iParseEras.get(text)
    if (era != null) {
      return era.intValue()
    }
    throw IllegalFieldValueException.create(DateTimeFieldType.era(), text)
  }

  def getEraMaxTextLength(): Int = iMaxEraLength

  def monthOfYearValueToText(value: Int): String = iMonths(value)

  def monthOfYearValueToShortText(value: Int): String = iShortMonths(value)

  def monthOfYearTextToValue(text: String): Int = {
    val month = iParseMonths.get(text)
    if (month != null) {
      return month.intValue()
    }
    throw IllegalFieldValueException.create(DateTimeFieldType.monthOfYear(), text)
  }

  def getMonthMaxTextLength(): Int = iMaxMonthLength

  def getMonthMaxShortTextLength(): Int = iMaxShortMonthLength

  def dayOfWeekValueToText(value: Int): String = iDaysOfWeek(value)

  def dayOfWeekValueToShortText(value: Int): String = iShortDaysOfWeek(value)

  def dayOfWeekTextToValue(text: String): Int = {
    val day = iParseDaysOfWeek.get(text)
    if (day != null) {
      return day.intValue()
    }
    throw IllegalFieldValueException.create(DateTimeFieldType.dayOfWeek(), text)
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
    throw IllegalFieldValueException.create(DateTimeFieldType.halfdayOfDay(), text)
  }

  def getHalfdayMaxTextLength(): Int = iMaxHalfdayLength
}
