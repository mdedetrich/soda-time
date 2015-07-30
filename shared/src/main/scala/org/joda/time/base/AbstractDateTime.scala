package org.joda.time.base

import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Locale
import org.joda.convert.ToString
import org.joda.time.DateTimeFieldType
import org.joda.time.ReadableDateTime
import org.joda.time.format.DateTimeFormat

abstract class AbstractDateTime extends AbstractInstant() with ReadableDateTime {

  override def get(`type`: DateTimeFieldType): Int = {
    if (`type` == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    `type`.getField(getChronology).get(getMillis)
  }

  def getEra(): Int = getChronology.era().get(getMillis)

  def getCenturyOfEra(): Int = {
    getChronology.centuryOfEra().get(getMillis)
  }

  def getYearOfEra(): Int = {
    getChronology.yearOfEra().get(getMillis)
  }

  def getYearOfCentury(): Int = {
    getChronology.yearOfCentury().get(getMillis)
  }

  def getYear(): Int = getChronology.year().get(getMillis)

  def getWeekyear(): Int = getChronology.weekyear().get(getMillis)

  def getMonthOfYear(): Int = {
    getChronology.monthOfYear().get(getMillis)
  }

  def getWeekOfWeekyear(): Int = {
    getChronology.weekOfWeekyear().get(getMillis)
  }

  def getDayOfYear(): Int = {
    getChronology.dayOfYear().get(getMillis)
  }

  def getDayOfMonth(): Int = {
    getChronology.dayOfMonth().get(getMillis)
  }

  def getDayOfWeek(): Int = {
    getChronology.dayOfWeek().get(getMillis)
  }

  def getHourOfDay(): Int = {
    getChronology.hourOfDay().get(getMillis)
  }

  def getMinuteOfDay(): Int = {
    getChronology.minuteOfDay().get(getMillis)
  }

  def getMinuteOfHour(): Int = {
    getChronology.minuteOfHour().get(getMillis)
  }

  def getSecondOfDay(): Int = {
    getChronology.secondOfDay().get(getMillis)
  }

  def getSecondOfMinute(): Int = {
    getChronology.secondOfMinute().get(getMillis)
  }

  def getMillisOfDay(): Int = {
    getChronology.millisOfDay().get(getMillis)
  }

  def getMillisOfSecond(): Int = {
    getChronology.millisOfSecond().get(getMillis)
  }

  def toCalendar(locale: Locale): Calendar = {
    var _locale:Locale = locale
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    val zone = getZone
    val cal = Calendar.getInstance(zone.toTimeZone(), _locale)
    cal.setTime(toDate())
    cal
  }

  def toGregorianCalendar(): GregorianCalendar = {
    val zone = getZone
    val cal = new GregorianCalendar(zone.toTimeZone())
    cal.setTime(toDate())
    cal
  }

  @ToString
  override def toString(): String = super.toString

  def toString(pattern: String): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).print(this)
  }

  def toString(pattern: String, locale: Locale): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).withLocale(locale)
      .print(this)
  }
}
