package org.joda.time

import org.joda.convert.FromString
import org.joda.convert.ToString
import org.joda.time.base.BaseSingleFieldPeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat

object Days {

  val ZERO = new Days(0)
  val ONE = new Days(1)
  val TWO = new Days(2)
  val THREE = new Days(3)
  val FOUR = new Days(4)
  val FIVE = new Days(5)
  val SIX = new Days(6)
  val SEVEN = new Days(7)
  val MAX_VALUE = new Days(Integer.MAX_VALUE)
  val MIN_VALUE = new Days(Integer.MIN_VALUE)

  private val PARSER = ISOPeriodFormat.standard().withParseType(PeriodType.days())

  def days(days: Int): Days = days match {
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case 4 => FOUR
    case 5 => FIVE
    case 6 => SIX
    case 7 => SEVEN
    case Integer.MAX_VALUE => MAX_VALUE
    case Integer.MIN_VALUE => MIN_VALUE
    case _ => new Days(days)
  }

  def daysBetween(start: ReadableInstant, end: ReadableInstant): Days = {
    val amount = BaseSingleFieldPeriod.between(start, end, DurationFieldType.days())
    Days.days(amount)
  }

  def daysBetween(start: ReadablePartial, end: ReadablePartial): Days = {
    if (start.isInstanceOf[LocalDate] && end.isInstanceOf[LocalDate]) {
      val chrono = DateTimeUtils.getChronology(start.getChronology)
      val days = chrono.days().getDifference(end.asInstanceOf[LocalDate].getLocalMillis, start.asInstanceOf[LocalDate].getLocalMillis)
      return Days.days(days)
    }
    val amount = BaseSingleFieldPeriod.between(start, end, ZERO)
    Days.days(amount)
  }

  def daysIn(interval: ReadableInterval): Days = {
    if (interval == null) {
      return Days.ZERO
    }
    val amount = BaseSingleFieldPeriod.between(interval.getStart, interval.getEnd, DurationFieldType.days())
    Days.days(amount)
  }

  def standardDaysIn(period: ReadablePeriod): Days = {
    val amount = BaseSingleFieldPeriod.standardPeriodIn(period, DateTimeConstants.MILLIS_PER_DAY)
    Days.days(amount)
  }

  @FromString
  def parseDays(periodStr: String): Days = {
    if (periodStr == null) {
      return Days.ZERO
    }
    val p = PARSER.parsePeriod(periodStr)
    Days.days(p.getDays)
  }
}

@SerialVersionUID(87525275727380865L)
class Days private (days: Int) extends BaseSingleFieldPeriod(days) {

  private def readResolve(): AnyRef = Days.days(getValue)

  def getFieldType(): DurationFieldType = DurationFieldType.days()

  def getPeriodType(): PeriodType = PeriodType.days()

  def toStandardWeeks(): Weeks = {
    Weeks.weeks(getValue / DateTimeConstants.DAYS_PER_WEEK)
  }

  def toStandardHours(): Hours = {
    Hours.hours(FieldUtils.safeMultiply(getValue, DateTimeConstants.HOURS_PER_DAY))
  }

  def toStandardMinutes(): Minutes = {
    Minutes.minutes(FieldUtils.safeMultiply(getValue, DateTimeConstants.MINUTES_PER_DAY))
  }

  def toStandardSeconds(): Seconds = {
    Seconds.seconds(FieldUtils.safeMultiply(getValue, DateTimeConstants.SECONDS_PER_DAY))
  }

  def toStandardDuration(): Duration = {
    val days = getValue
    new Duration(days * DateTimeConstants.MILLIS_PER_DAY)
  }

  def getDays(): Int = getValue

  def plus(days: Int): Days = {
    if (days == 0) {
      return this
    }
    Days.days(FieldUtils.safeAdd(getValue, days))
  }

  def plus(days: Days): Days = {
    if (days == null) {
      return this
    }
    plus(days.getValue)
  }

  def minus(days: Int): Days = plus(FieldUtils.safeNegate(days))

  def minus(days: Days): Days = {
    if (days == null) {
      return this
    }
    minus(days.getValue)
  }

  def multipliedBy(scalar: Int): Days = {
    Days.days(FieldUtils.safeMultiply(getValue, scalar))
  }

  def dividedBy(divisor: Int): Days = {
    if (divisor == 1) {
      return this
    }
    Days.days(getValue / divisor)
  }

  def negated(): Days = {
    Days.days(FieldUtils.safeNegate(getValue))
  }

  def isGreaterThan(other: Days): Boolean = {
    if (other == null) {
      return getValue > 0
    }
    getValue > other.getValue
  }

  def isLessThan(other: Days): Boolean = {
    if (other == null) {
      return getValue < 0
    }
    getValue < other.getValue
  }

  @ToString
  override def toString(): String = "P" + String.valueOf(getValue) + "D"
}
