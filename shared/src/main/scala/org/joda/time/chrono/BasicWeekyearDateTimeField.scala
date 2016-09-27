package org.joda.time.chrono

import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.field.FieldUtils
import org.joda.time.field.ImpreciseDateTimeField
import BasicWeekyearDateTimeField._

object BasicWeekyearDateTimeField {

  private val WEEK_53 = (53L - 1) * DateTimeConstants.MILLIS_PER_WEEK
}

@SerialVersionUID(6215066916806820644L)
class BasicWeekyearDateTimeField(private val chronology: BasicChronology)
    extends ImpreciseDateTimeField(DateTimeFieldType.weekyear(),
                                   chronology.getAverageMillisPerYear) {

  private val iChronology: BasicChronology = chronology

  def isLenient(): Boolean = false

  def get(instant: Long): Int = iChronology.getWeekyear(instant)

  override def add(instant: Long, years: Int): Long = {
    if (years == 0) {
      return instant
    }
    set(instant, get(instant) + years)
  }

  override def add(instant: Long, value: Long): Long = {
    add(instant, FieldUtils.safeToInt(value))
  }

  override def addWrapField(instant: Long, years: Int): Long =
    add(instant, years)

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    if (minuendInstant < subtrahendInstant) {
      return -getDifference(subtrahendInstant, minuendInstant)
    }
    val minuendWeekyear = get(minuendInstant)
    val subtrahendWeekyear = get(subtrahendInstant)
    val minuendRem = remainder(minuendInstant)
    var subtrahendRem = remainder(subtrahendInstant)
    if (subtrahendRem >= WEEK_53 && iChronology.getWeeksInYear(minuendWeekyear) <= 52) {
      subtrahendRem -= DateTimeConstants.MILLIS_PER_WEEK
    }
    var difference = minuendWeekyear - subtrahendWeekyear
    if (minuendRem < subtrahendRem) {
      difference -= 1
    }
    difference
  }

  def set(instant: Long, year: Int): Long = {
    FieldUtils.verifyValueBounds(this,
                                 Math.abs(year),
                                 iChronology.getMinYear,
                                 iChronology.getMaxYear)
    val thisWeekyear = get(instant)
    if (thisWeekyear == year) {
      return instant
    }
    val thisDow = iChronology.getDayOfWeek(instant)
    val weeksInFromYear = iChronology.getWeeksInYear(thisWeekyear)
    val weeksInToYear = iChronology.getWeeksInYear(year)
    val maxOutWeeks =
      if ((weeksInToYear < weeksInFromYear)) weeksInToYear else weeksInFromYear
    var setToWeek = iChronology.getWeekOfWeekyear(instant)
    if (setToWeek > maxOutWeeks) {
      setToWeek = maxOutWeeks
    }
    var workInstant = instant
    workInstant = iChronology.setYear(workInstant, year)
    val workWoyYear = get(workInstant)
    if (workWoyYear < year) {
      workInstant += DateTimeConstants.MILLIS_PER_WEEK
    } else if (workWoyYear > year) {
      workInstant -= DateTimeConstants.MILLIS_PER_WEEK
    }
    val currentWoyWeek = iChronology.getWeekOfWeekyear(workInstant)
    workInstant = workInstant +
        (setToWeek - currentWoyWeek) * DateTimeConstants.MILLIS_PER_WEEK.toLong
    workInstant = iChronology.dayOfWeek().set(workInstant, thisDow)
    workInstant
  }

  def getRangeDurationField(): DurationField = null

  override def isLeap(instant: Long): Boolean = {
    iChronology.getWeeksInYear(iChronology.getWeekyear(instant)) >
      52
  }

  override def getLeapAmount(instant: Long): Int = {
    iChronology.getWeeksInYear(iChronology.getWeekyear(instant)) -
      52
  }

  override def getLeapDurationField(): DurationField = iChronology.weeks()

  def getMinimumValue(): Int = iChronology.getMinYear

  def getMaximumValue(): Int = iChronology.getMaxYear

  def roundFloor(instant: Long): Long = {
    var _instant: Long = instant
    _instant = iChronology.weekOfWeekyear().roundFloor(_instant)
    val wow = iChronology.getWeekOfWeekyear(_instant)
    if (wow > 1) {
      _instant -= DateTimeConstants.MILLIS_PER_WEEK.toLong * (wow - 1)
    }
    _instant
  }

  override def remainder(instant: Long): Long = instant - roundFloor(instant)

  private def readResolve(): AnyRef = iChronology.weekyear()
}
