package org.joda.time.chrono

import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.FieldUtils
import org.joda.time.field.ImpreciseDateTimeField
import BasicMonthOfYearDateTimeField._

object BasicMonthOfYearDateTimeField {

  private val MIN = DateTimeConstants.JANUARY
}

@SerialVersionUID(-8258715387168736L)
class BasicMonthOfYearDateTimeField(private val chronology: BasicChronology, private val leapMonth: Int)
  extends ImpreciseDateTimeField(DateTimeFieldType.monthOfYear(), chronology.getAverageMillisPerMonth) {

  private var iChronology: BasicChronology = null
  private var iMax:Int = _
  private var iLeapMonth: Int = _

  iChronology = chronology
  iMax = iChronology.getMaxMonth
  iLeapMonth = leapMonth
  
  def isLenient(): Boolean = false

  def get(instant: Long): Int = iChronology.getMonthOfYear(instant)

  override def add(instant: Long, months: Int): Long = {
    if (months == 0) {
      return instant
    }
    val timePart = iChronology.getMillisOfDay(instant)
    val thisYear = iChronology.getYear(instant)
    val thisMonth = iChronology.getMonthOfYear(instant, thisYear)
    var yearToUse: Int = 0
    var monthToUse = thisMonth - 1 + months
    if (monthToUse >= 0) {
      yearToUse = thisYear + (monthToUse / iMax)
      monthToUse = (monthToUse % iMax) + 1
    } else {
      yearToUse = thisYear + (monthToUse / iMax) - 1
      monthToUse = Math.abs(monthToUse)
      var remMonthToUse = monthToUse % iMax
      if (remMonthToUse == 0) {
        remMonthToUse = iMax
      }
      monthToUse = iMax - remMonthToUse + 1
      if (monthToUse == 1) {
        yearToUse += 1
      }
    }
    var dayToUse = iChronology.getDayOfMonth(instant, thisYear, thisMonth)
    val maxDay = iChronology.getDaysInYearMonth(yearToUse, monthToUse)
    if (dayToUse > maxDay) {
      dayToUse = maxDay
    }
    val datePart = iChronology.getYearMonthDayMillis(yearToUse, monthToUse, dayToUse)
    datePart + timePart
  }

  override def add(instant: Long, months: Long): Long = {
    val i_months = months.toInt
    if (i_months == months) {
      return add(instant, i_months)
    }
    val timePart = iChronology.getMillisOfDay(instant)
    val thisYear = iChronology.getYear(instant)
    val thisMonth = iChronology.getMonthOfYear(instant, thisYear)
    var yearToUse: Long = 0l
    var monthToUse = thisMonth - 1 + months
    if (monthToUse >= 0) {
      yearToUse = thisYear + (monthToUse / iMax)
      monthToUse = (monthToUse % iMax) + 1
    } else {
      yearToUse = thisYear + (monthToUse / iMax) - 1
      monthToUse = Math.abs(monthToUse)
      var remMonthToUse = (monthToUse % iMax).toInt
      if (remMonthToUse == 0) {
        remMonthToUse = iMax
      }
      monthToUse = iMax - remMonthToUse + 1
      if (monthToUse == 1) {
        yearToUse += 1
      }
    }
    if (yearToUse < iChronology.getMinYear || yearToUse > iChronology.getMaxYear) {
      throw new IllegalArgumentException("Magnitude of add amount is too large: " + months)
    }
    val i_yearToUse = yearToUse.toInt
    val i_monthToUse = monthToUse.toInt
    var dayToUse = iChronology.getDayOfMonth(instant, thisYear, thisMonth)
    val maxDay = iChronology.getDaysInYearMonth(i_yearToUse, i_monthToUse)
    if (dayToUse > maxDay) {
      dayToUse = maxDay
    }
    val datePart = iChronology.getYearMonthDayMillis(i_yearToUse, i_monthToUse, dayToUse)
    datePart + timePart
  }

  override def add(partial: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          valueToAdd: Int): Array[Int] = {
    if (valueToAdd == 0) {
      return values
    }
    if (partial.size > 0 &&
      partial.getFieldType(0) == DateTimeFieldType.monthOfYear() &&
      fieldIndex == 0) {
      val curMonth0 = partial.getValue(0) - 1
      val newMonth = ((curMonth0 + (valueToAdd % 12) + 12) % 12) + 1
      return set(partial, 0, values, newMonth)
    }
    if (DateTimeUtils.isContiguous(partial)) {
      var instant = 0L
      for (i <- 0 until partial.size()) {
        instant = partial.getFieldType(i).getField(iChronology).set(instant, values(i))
      }
      instant = add(instant, valueToAdd)
      iChronology.get(partial, instant)
    } else {
      super.add(partial, fieldIndex, values, valueToAdd)
    }
  }

  override def addWrapField(instant: Long, months: Int): Long = {
    set(instant, FieldUtils.getWrappedValue(get(instant), months, MIN, iMax))
  }

  override def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
    var _subtrahendInstant: Long = subtrahendInstant
    if (minuendInstant < _subtrahendInstant) {
      return -getDifference(_subtrahendInstant, minuendInstant)
    }
    val minuendYear = iChronology.getYear(minuendInstant)
    val minuendMonth = iChronology.getMonthOfYear(minuendInstant, minuendYear)
    val subtrahendYear = iChronology.getYear(_subtrahendInstant)
    val subtrahendMonth = iChronology.getMonthOfYear(_subtrahendInstant, subtrahendYear)
    var difference = (minuendYear - subtrahendYear) * iMax.toLong + minuendMonth -
      subtrahendMonth
    val minuendDom = iChronology.getDayOfMonth(minuendInstant, minuendYear, minuendMonth)
    if (minuendDom ==
      iChronology.getDaysInYearMonth(minuendYear, minuendMonth)) {
      val subtrahendDom = iChronology.getDayOfMonth(_subtrahendInstant, subtrahendYear, subtrahendMonth)
      if (subtrahendDom > minuendDom) {
        _subtrahendInstant = iChronology.dayOfMonth().set(_subtrahendInstant, minuendDom)
      }
    }
    val minuendRem = minuendInstant -
      iChronology.getYearMonthMillis(minuendYear, minuendMonth)
    val subtrahendRem = _subtrahendInstant -
      iChronology.getYearMonthMillis(subtrahendYear, subtrahendMonth)
    if (minuendRem < subtrahendRem) {
      difference -= 1
    }
    difference
  }

  def set(instant: Long, month: Int): Long = {
    FieldUtils.verifyValueBounds(this, month, MIN, iMax)
    val thisYear = iChronology.getYear(instant)
    var thisDom = iChronology.getDayOfMonth(instant, thisYear)
    val maxDom = iChronology.getDaysInYearMonth(thisYear, month)
    if (thisDom > maxDom) {
      thisDom = maxDom
    }
    iChronology.getYearMonthDayMillis(thisYear, month, thisDom) +
      iChronology.getMillisOfDay(instant)
  }

  def getRangeDurationField(): DurationField = iChronology.years()

  override def isLeap(instant: Long): Boolean = {
    val thisYear = iChronology.getYear(instant)
    if (iChronology.isLeapYear(thisYear)) {
      return iChronology.getMonthOfYear(instant, thisYear) == iLeapMonth
    }
    false
  }

  override def getLeapAmount(instant: Long): Int = if (isLeap(instant)) 1 else 0

  override def getLeapDurationField(): DurationField = iChronology.days()

  def getMinimumValue(): Int = MIN

  def getMaximumValue(): Int = iMax

  def roundFloor(instant: Long): Long = {
    val year = iChronology.getYear(instant)
    val month = iChronology.getMonthOfYear(instant, year)
    iChronology.getYearMonthMillis(year, month)
  }

  override def remainder(instant: Long): Long = instant - roundFloor(instant)

  private def readResolve(): AnyRef = iChronology.monthOfYear()
}
