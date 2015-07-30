package org.joda.time.chrono

import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import BasicGJChronology._

object BasicGJChronology {

  private val MIN_DAYS_PER_MONTH_ARRAY = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val MAX_DAYS_PER_MONTH_ARRAY = Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val MIN_TOTAL_MILLIS_BY_MONTH_ARRAY = new Array[Long](12)
  private val MAX_TOTAL_MILLIS_BY_MONTH_ARRAY = new Array[Long](12)
  private val FEB_29 = (31L + 29 - 1) * DateTimeConstants.MILLIS_PER_DAY

  var minSum:Long = 0

  var maxSum:Long = 0

  for (i <- 0 until 11) {
    var millis = MIN_DAYS_PER_MONTH_ARRAY(i) * DateTimeConstants.MILLIS_PER_DAY.toLong
    minSum += millis
    MIN_TOTAL_MILLIS_BY_MONTH_ARRAY(i + 1) = minSum
    millis = MAX_DAYS_PER_MONTH_ARRAY(i) * DateTimeConstants.MILLIS_PER_DAY.toLong
    maxSum += millis
    MAX_TOTAL_MILLIS_BY_MONTH_ARRAY(i + 1) = maxSum
  }
}

@SerialVersionUID(538276888268L)
abstract class BasicGJChronology(base: Chronology, param: AnyRef, minDaysInFirstWeek: Int)
  extends BasicChronology(base, param, minDaysInFirstWeek) {

  override def isLeapDay(instant: Long): Boolean = {
    dayOfMonth().get(instant) == 29 && monthOfYear().isLeap(instant)
  }

  def getMonthOfYear(millis: Long, year: Int): Int = {
    val i = ((millis - getYearMillis(year)) >> 10).toInt
    if (isLeapYear(year)) 
      if (i < 182 * 84375) 
        if (i < 91 * 84375) 
          if (i < 31 * 84375) 1 
          else if (i < 60 * 84375) 2 
          else 3 
        else if (i < 121 * 84375) 4 
        else if (i < 152 * 84375) 5 
        else 6 
      else if (i < 274 * 84375) 
        if (i < 213 * 84375) 7 
        else if (i < 244 * 84375) 8 
        else 9 
      else if (i < 305 * 84375) 10 
      else if (i < 335 * 84375) 11 
      else 12 
    else if (i < 181 * 84375) 
      if (i < 90 * 84375) 
        if (i < 31 * 84375) 1 
        else if (i < 59 * 84375) 2 
        else 3 
      else if (i < 120 * 84375) 4 
      else if (i < 151 * 84375) 5 
      else 6 
    else if (i < 273 * 84375) 
      if (i < 212 * 84375) 7 
      else if (i < 243 * 84375) 8 
      else 9 
    else if (i < 304 * 84375) 10 
    else if (i < 334 * 84375) 11 
    else 12
  }

  def getDaysInYearMonth(year: Int, month: Int): Int = {
    if (isLeapYear(year)) {
      MAX_DAYS_PER_MONTH_ARRAY(month - 1)
    } else {
      MIN_DAYS_PER_MONTH_ARRAY(month - 1)
    }
  }

  def getDaysInMonthMax(month: Int): Int = MAX_DAYS_PER_MONTH_ARRAY(month - 1)

  override def getDaysInMonthMaxForSet(instant: Long, value: Int): Int = {
    (if ((value > 28 || value < 1)) getDaysInMonthMax(instant) else 28)
  }

  def getTotalMillisByYearMonth(year: Int, month: Int): Long = {
    if (isLeapYear(year)) {
      MAX_TOTAL_MILLIS_BY_MONTH_ARRAY(month - 1)
    } else {
      MIN_TOTAL_MILLIS_BY_MONTH_ARRAY(month - 1)
    }
  }

  def getYearDifference(minuendInstant: Long, subtrahendInstant: Long): Long = {
    val minuendYear = getYear(minuendInstant)
    val subtrahendYear = getYear(subtrahendInstant)
    var minuendRem = minuendInstant - getYearMillis(minuendYear)
    var subtrahendRem = subtrahendInstant - getYearMillis(subtrahendYear)
    if (subtrahendRem >= FEB_29) {
      if (isLeapYear(subtrahendYear)) {
        if (!isLeapYear(minuendYear)) {
          subtrahendRem -= DateTimeConstants.MILLIS_PER_DAY
        }
      } else if (minuendRem >= FEB_29 && isLeapYear(minuendYear)) {
        minuendRem -= DateTimeConstants.MILLIS_PER_DAY
      }
    }
    var difference = minuendYear - subtrahendYear
    if (minuendRem < subtrahendRem) {
      difference -= 1
    }
    difference
  }

  def setYear(instant: Long, year: Int): Long = {
    var _instant: Long = instant
    val thisYear = getYear(_instant)
    var dayOfYear = getDayOfYear(_instant, thisYear)
    val millisOfDay = getMillisOfDay(_instant)
    if (dayOfYear > (31 + 28)) {
      if (isLeapYear(thisYear)) {
        if (!isLeapYear(year)) {
          dayOfYear -= 1
        }
      } else {
        if (isLeapYear(year)) {
          dayOfYear += 1
        }
      }
    }
    _instant = getYearMonthDayMillis(year, 1, dayOfYear)
    _instant += millisOfDay
    _instant
  }
}
