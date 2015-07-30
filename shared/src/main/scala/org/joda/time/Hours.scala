package org.joda.time

import org.joda.convert.FromString
import org.joda.convert.ToString
import org.joda.time.base.BaseSingleFieldPeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat

object Hours {

  val ZERO = new Hours(0)
  val ONE = new Hours(1)
  val TWO = new Hours(2)
  val THREE = new Hours(3)
  val FOUR = new Hours(4)
  val FIVE = new Hours(5)
  val SIX = new Hours(6)
  val SEVEN = new Hours(7)
  val EIGHT = new Hours(8)
  val MAX_VALUE = new Hours(Integer.MAX_VALUE)
  val MIN_VALUE = new Hours(Integer.MIN_VALUE)

  private val PARSER = ISOPeriodFormat.standard().withParseType(PeriodType.hours())

  def hours(hours: Int): Hours = hours match {
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case 4 => FOUR
    case 5 => FIVE
    case 6 => SIX
    case 7 => SEVEN
    case 8 => EIGHT
    case Integer.MAX_VALUE => MAX_VALUE
    case Integer.MIN_VALUE => MIN_VALUE
    case _ => new Hours(hours)
  }

  def hoursBetween(start: ReadableInstant, end: ReadableInstant): Hours = {
    val amount = BaseSingleFieldPeriod.between(start, end, DurationFieldType.hours())
    Hours.hours(amount)
  }

  def hoursBetween(start: ReadablePartial, end: ReadablePartial): Hours = {
    if (start.isInstanceOf[LocalTime] && end.isInstanceOf[LocalTime]) {
      val chrono = DateTimeUtils.getChronology(start.getChronology)
      val hours = chrono.hours().getDifference(end.asInstanceOf[LocalTime].getLocalMillis, start.asInstanceOf[LocalTime].getLocalMillis)
      return Hours.hours(hours)
    }
    val amount = BaseSingleFieldPeriod.between(start, end, ZERO)
    Hours.hours(amount)
  }

  def hoursIn(interval: ReadableInterval): Hours = {
    if (interval == null) {
      return Hours.ZERO
    }
    val amount = BaseSingleFieldPeriod.between(interval.getStart, interval.getEnd, DurationFieldType.hours())
    Hours.hours(amount)
  }

  def standardHoursIn(period: ReadablePeriod): Hours = {
    val amount = BaseSingleFieldPeriod.standardPeriodIn(period, DateTimeConstants.MILLIS_PER_HOUR)
    Hours.hours(amount)
  }

  @FromString
  def parseHours(periodStr: String): Hours = {
    if (periodStr == null) {
      return Hours.ZERO
    }
    val p = PARSER.parsePeriod(periodStr)
    Hours.hours(p.getHours)
  }
}

@SerialVersionUID(87525275727380864L)
class Hours private (hours: Int) extends BaseSingleFieldPeriod(hours) {

  private def readResolve(): AnyRef = Hours.hours(getValue)

  def getFieldType(): DurationFieldType = DurationFieldType.hours()

  def getPeriodType(): PeriodType = PeriodType.hours()

  def toStandardWeeks(): Weeks = {
    Weeks.weeks(getValue / DateTimeConstants.HOURS_PER_WEEK)
  }

  def toStandardDays(): Days = {
    Days.days(getValue / DateTimeConstants.HOURS_PER_DAY)
  }

  def toStandardMinutes(): Minutes = {
    Minutes.minutes(FieldUtils.safeMultiply(getValue, DateTimeConstants.MINUTES_PER_HOUR))
  }

  def toStandardSeconds(): Seconds = {
    Seconds.seconds(FieldUtils.safeMultiply(getValue, DateTimeConstants.SECONDS_PER_HOUR))
  }

  def toStandardDuration(): Duration = {
    val hours = getValue
    new Duration(hours * DateTimeConstants.MILLIS_PER_HOUR)
  }

  def getHours(): Int = getValue

  def plus(hours: Int): Hours = {
    if (hours == 0) {
      return this
    }
    Hours.hours(FieldUtils.safeAdd(getValue, hours))
  }

  def plus(hours: Hours): Hours = {
    if (hours == null) {
      return this
    }
    plus(hours.getValue)
  }

  def minus(hours: Int): Hours = plus(FieldUtils.safeNegate(hours))

  def minus(hours: Hours): Hours = {
    if (hours == null) {
      return this
    }
    minus(hours.getValue)
  }

  def multipliedBy(scalar: Int): Hours = {
    Hours.hours(FieldUtils.safeMultiply(getValue, scalar))
  }

  def dividedBy(divisor: Int): Hours = {
    if (divisor == 1) {
      return this
    }
    Hours.hours(getValue / divisor)
  }

  def negated(): Hours = {
    Hours.hours(FieldUtils.safeNegate(getValue))
  }

  def isGreaterThan(other: Hours): Boolean = {
    if (other == null) {
      return getValue > 0
    }
    getValue > other.getValue
  }

  def isLessThan(other: Hours): Boolean = {
    if (other == null) {
      return getValue < 0
    }
    getValue < other.getValue
  }

  @ToString
  override def toString(): String = "PT" + String.valueOf(getValue) + "H"
}
