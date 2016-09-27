package org.joda.time.chrono

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.field.FieldUtils
import org.joda.time.field.ImpreciseDateTimeField

@SerialVersionUID(-98628754872287L)
class BasicYearDateTimeField(protected val chronology: BasicChronology)
    extends ImpreciseDateTimeField(DateTimeFieldType.year(),
                                   chronology.getAverageMillisPerYear) {

  private var iChronology: BasicChronology = null

  iChronology = chronology

  def isLenient(): Boolean = false

  def get(instant: Long): Int = iChronology.getYear(instant)

  override def add(instant: Long, years: Int): Long = {
    if (years == 0) {
      return instant
    }
    val thisYear = get(instant)
    val newYear = FieldUtils.safeAdd(thisYear, years)
    set(instant, newYear)
  }

  override def add(instant: Long, years: Long): Long = {
    add(instant, FieldUtils.safeToInt(years))
  }

  override def addWrapField(instant: Long, years: Int): Long = {
    if (years == 0) {
      return instant
    }
    val thisYear = iChronology.getYear(instant)
    val wrappedYear = FieldUtils.getWrappedValue(thisYear,
                                                 years,
                                                 iChronology.getMinYear,
                                                 iChronology.getMaxYear)
    set(instant, wrappedYear)
  }

  def set(instant: Long, year: Int): Long = {
    FieldUtils.verifyValueBounds(this,
                                 year,
                                 iChronology.getMinYear,
                                 iChronology.getMaxYear)
    iChronology.setYear(instant, year)
  }

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    if (minuendInstant < subtrahendInstant) {
      return -iChronology.getYearDifference(subtrahendInstant, minuendInstant)
    }
    iChronology.getYearDifference(minuendInstant, subtrahendInstant)
  }

  def getRangeDurationField(): DurationField = null

  override def isLeap(instant: Long): Boolean =
    iChronology.isLeapYear(get(instant))

  override def getLeapAmount(instant: Long): Int = {
    if (iChronology.isLeapYear(get(instant))) {
      1
    } else {
      0
    }
  }

  override def getLeapDurationField(): DurationField = iChronology.days()

  def getMinimumValue(): Int = iChronology.getMinYear

  def getMaximumValue(): Int = iChronology.getMaxYear

  def roundFloor(instant: Long): Long = iChronology.getYearMillis(get(instant))

  override def roundCeiling(instant: Long): Long = {
    var _instant: Long = instant
    val year = get(_instant)
    val yearStartMillis = iChronology.getYearMillis(year)
    if (_instant != yearStartMillis) {
      _instant = iChronology.getYearMillis(year + 1)
    }
    _instant
  }

  override def remainder(instant: Long): Long = instant - roundFloor(instant)

  private def readResolve(): AnyRef = iChronology.year()
}
