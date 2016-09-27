package org.joda.time

import org.joda.convert.FromString
import org.joda.convert.ToString
import org.joda.time.base.BaseSingleFieldPeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat

object Minutes {

  val ZERO = new Minutes(0)
  val ONE = new Minutes(1)
  val TWO = new Minutes(2)
  val THREE = new Minutes(3)
  val MAX_VALUE = new Minutes(Integer.MAX_VALUE)
  val MIN_VALUE = new Minutes(Integer.MIN_VALUE)

  private val PARSER =
    ISOPeriodFormat.standard().withParseType(PeriodType.minutes())

  def minutes(minutes: Int): Minutes = minutes match {
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case Integer.MAX_VALUE => MAX_VALUE
    case Integer.MIN_VALUE => MIN_VALUE
    case _ => new Minutes(minutes)
  }

  def minutesBetween(start: ReadableInstant, end: ReadableInstant): Minutes = {
    val amount =
      BaseSingleFieldPeriod.between(start, end, DurationFieldType.minutes())
    Minutes.minutes(amount)
  }

  def minutesBetween(start: ReadablePartial, end: ReadablePartial): Minutes = {
    if (start.isInstanceOf[LocalTime] && end.isInstanceOf[LocalTime]) {
      val chrono = DateTimeUtils.getChronology(start.getChronology)
      val minutes = chrono
        .minutes()
        .getDifference(end.asInstanceOf[LocalTime].getLocalMillis,
                       start.asInstanceOf[LocalTime].getLocalMillis)
      return Minutes.minutes(minutes)
    }
    val amount = BaseSingleFieldPeriod.between(start, end, ZERO)
    Minutes.minutes(amount)
  }

  def minutesIn(interval: ReadableInterval): Minutes = {
    if (interval == null) {
      return Minutes.ZERO
    }
    val amount = BaseSingleFieldPeriod
      .between(interval.getStart, interval.getEnd, DurationFieldType.minutes())
    Minutes.minutes(amount)
  }

  def standardMinutesIn(period: ReadablePeriod): Minutes = {
    val amount = BaseSingleFieldPeriod
      .standardPeriodIn(period, DateTimeConstants.MILLIS_PER_MINUTE)
    Minutes.minutes(amount)
  }

  @FromString
  def parseMinutes(periodStr: String): Minutes = {
    if (periodStr == null) {
      return Minutes.ZERO
    }
    val p = PARSER.parsePeriod(periodStr)
    Minutes.minutes(p.getMinutes)
  }
}

@SerialVersionUID(87525275727380863L)
class Minutes private (minutes: Int) extends BaseSingleFieldPeriod(minutes) {

  private def readResolve(): AnyRef = Minutes.minutes(getValue)

  def getFieldType(): DurationFieldType = DurationFieldType.minutes()

  def getPeriodType(): PeriodType = PeriodType.minutes()

  def toStandardWeeks(): Weeks = {
    Weeks.weeks(getValue / DateTimeConstants.MINUTES_PER_WEEK)
  }

  def toStandardDays(): Days = {
    Days.days(getValue / DateTimeConstants.MINUTES_PER_DAY)
  }

  def toStandardHours(): Hours = {
    Hours.hours(getValue / DateTimeConstants.MINUTES_PER_HOUR)
  }

  def toStandardSeconds(): Seconds = {
    Seconds.seconds(
      FieldUtils.safeMultiply(getValue, DateTimeConstants.SECONDS_PER_MINUTE))
  }

  def toStandardDuration(): Duration = {
    val minutes = getValue
    new Duration(minutes * DateTimeConstants.MILLIS_PER_MINUTE)
  }

  def getMinutes(): Int = getValue

  def plus(minutes: Int): Minutes = {
    if (minutes == 0) {
      return this
    }
    Minutes.minutes(FieldUtils.safeAdd(getValue, minutes))
  }

  def plus(minutes: Minutes): Minutes = {
    if (minutes == null) {
      return this
    }
    plus(minutes.getValue)
  }

  def minus(minutes: Int): Minutes = plus(FieldUtils.safeNegate(minutes))

  def minus(minutes: Minutes): Minutes = {
    if (minutes == null) {
      return this
    }
    minus(minutes.getValue)
  }

  def multipliedBy(scalar: Int): Minutes = {
    Minutes.minutes(FieldUtils.safeMultiply(getValue, scalar))
  }

  def dividedBy(divisor: Int): Minutes = {
    if (divisor == 1) {
      return this
    }
    Minutes.minutes(getValue / divisor)
  }

  def negated(): Minutes = {
    Minutes.minutes(FieldUtils.safeNegate(getValue))
  }

  def isGreaterThan(other: Minutes): Boolean = {
    if (other == null) {
      return getValue > 0
    }
    getValue > other.getValue
  }

  def isLessThan(other: Minutes): Boolean = {
    if (other == null) {
      return getValue < 0
    }
    getValue < other.getValue
  }

  @ToString
  override def toString(): String = "PT" + String.valueOf(getValue) + "M"
}
