package org.joda.time

import org.joda.convert.FromString
import org.joda.convert.ToString
import org.joda.time.base.BaseSingleFieldPeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat

object Weeks {

  val ZERO = new Weeks(0)
  val ONE = new Weeks(1)
  val TWO = new Weeks(2)
  val THREE = new Weeks(3)
  val MAX_VALUE = new Weeks(Integer.MAX_VALUE)
  val MIN_VALUE = new Weeks(Integer.MIN_VALUE)

  private val PARSER =
    ISOPeriodFormat.standard().withParseType(PeriodType.weeks())

  def weeks(weeks: Int): Weeks = weeks match {
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case Integer.MAX_VALUE => MAX_VALUE
    case Integer.MIN_VALUE => MIN_VALUE
    case _ => new Weeks(weeks)
  }

  def weeksBetween(start: ReadableInstant, end: ReadableInstant): Weeks = {
    val amount =
      BaseSingleFieldPeriod.between(start, end, DurationFieldType.weeks())
    Weeks.weeks(amount)
  }

  def weeksBetween(start: ReadablePartial, end: ReadablePartial): Weeks = {
    if (start.isInstanceOf[LocalDate] && end.isInstanceOf[LocalDate]) {
      val chrono = DateTimeUtils.getChronology(start.getChronology)
      val weeks = chrono
        .weeks()
        .getDifference(end.asInstanceOf[LocalDate].getLocalMillis,
                       start.asInstanceOf[LocalDate].getLocalMillis)
      return Weeks.weeks(weeks)
    }
    val amount = BaseSingleFieldPeriod.between(start, end, ZERO)
    Weeks.weeks(amount)
  }

  def weeksIn(interval: ReadableInterval): Weeks = {
    if (interval == null) {
      return Weeks.ZERO
    }
    val amount = BaseSingleFieldPeriod
      .between(interval.getStart, interval.getEnd, DurationFieldType.weeks())
    Weeks.weeks(amount)
  }

  def standardWeeksIn(period: ReadablePeriod): Weeks = {
    val amount = BaseSingleFieldPeriod
      .standardPeriodIn(period, DateTimeConstants.MILLIS_PER_WEEK)
    Weeks.weeks(amount)
  }

  @FromString
  def parseWeeks(periodStr: String): Weeks = {
    if (periodStr == null) {
      return Weeks.ZERO
    }
    val p = PARSER.parsePeriod(periodStr)
    Weeks.weeks(p.getWeeks)
  }
}

@SerialVersionUID(87525275727380866L)
class Weeks private (weeks: Int) extends BaseSingleFieldPeriod(weeks) {

  private def readResolve(): AnyRef = Weeks.weeks(getValue)

  def getFieldType(): DurationFieldType = DurationFieldType.weeks()

  def getPeriodType(): PeriodType = PeriodType.weeks()

  def toStandardDays(): Days = {
    Days.days(
      FieldUtils.safeMultiply(getValue, DateTimeConstants.DAYS_PER_WEEK))
  }

  def toStandardHours(): Hours = {
    Hours.hours(
      FieldUtils.safeMultiply(getValue, DateTimeConstants.HOURS_PER_WEEK))
  }

  def toStandardMinutes(): Minutes = {
    Minutes.minutes(
      FieldUtils.safeMultiply(getValue, DateTimeConstants.MINUTES_PER_WEEK))
  }

  def toStandardSeconds(): Seconds = {
    Seconds.seconds(
      FieldUtils.safeMultiply(getValue, DateTimeConstants.SECONDS_PER_WEEK))
  }

  def toStandardDuration(): Duration = {
    val weeks = getValue
    new Duration(weeks * DateTimeConstants.MILLIS_PER_WEEK)
  }

  def getWeeks(): Int = getValue

  def plus(weeks: Int): Weeks = {
    if (weeks == 0) {
      return this
    }
    Weeks.weeks(FieldUtils.safeAdd(getValue, weeks))
  }

  def plus(weeks: Weeks): Weeks = {
    if (weeks == null) {
      return this
    }
    plus(weeks.getValue)
  }

  def minus(weeks: Int): Weeks = plus(FieldUtils.safeNegate(weeks))

  def minus(weeks: Weeks): Weeks = {
    if (weeks == null) {
      return this
    }
    minus(weeks.getValue)
  }

  def multipliedBy(scalar: Int): Weeks = {
    Weeks.weeks(FieldUtils.safeMultiply(getValue, scalar))
  }

  def dividedBy(divisor: Int): Weeks = {
    if (divisor == 1) {
      return this
    }
    Weeks.weeks(getValue / divisor)
  }

  def negated(): Weeks = {
    Weeks.weeks(FieldUtils.safeNegate(getValue))
  }

  def isGreaterThan(other: Weeks): Boolean = {
    if (other == null) {
      return getValue > 0
    }
    getValue > other.getValue
  }

  def isLessThan(other: Weeks): Boolean = {
    if (other == null) {
      return getValue < 0
    }
    getValue < other.getValue
  }

  @ToString
  override def toString(): String = "P" + String.valueOf(getValue) + "W"
}
