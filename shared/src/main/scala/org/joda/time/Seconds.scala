package org.joda.time

import org.joda.convert.FromString
import org.joda.convert.ToString
import org.joda.time.base.BaseSingleFieldPeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat

object Seconds {

  val ZERO = new Seconds(0)
  val ONE = new Seconds(1)
  val TWO = new Seconds(2)
  val THREE = new Seconds(3)
  val MAX_VALUE = new Seconds(Integer.MAX_VALUE)
  val MIN_VALUE = new Seconds(Integer.MIN_VALUE)

  private val PARSER =
    ISOPeriodFormat.standard().withParseType(PeriodType.seconds())

  def seconds(seconds: Int): Seconds = seconds match {
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case Integer.MAX_VALUE => MAX_VALUE
    case Integer.MIN_VALUE => MIN_VALUE
    case _ => new Seconds(seconds)
  }

  def secondsBetween(start: ReadableInstant, end: ReadableInstant): Seconds = {
    val amount =
      BaseSingleFieldPeriod.between(start, end, DurationFieldType.seconds())
    Seconds.seconds(amount)
  }

  def secondsBetween(start: ReadablePartial, end: ReadablePartial): Seconds = {
    if (start.isInstanceOf[LocalTime] && end.isInstanceOf[LocalTime]) {
      val chrono = DateTimeUtils.getChronology(start.getChronology)
      val seconds = chrono
        .seconds()
        .getDifference(end.asInstanceOf[LocalTime].getLocalMillis,
                       start.asInstanceOf[LocalTime].getLocalMillis)
      return Seconds.seconds(seconds)
    }
    val amount = BaseSingleFieldPeriod.between(start, end, ZERO)
    Seconds.seconds(amount)
  }

  def secondsIn(interval: ReadableInterval): Seconds = {
    if (interval == null) {
      return Seconds.ZERO
    }
    val amount = BaseSingleFieldPeriod
      .between(interval.getStart, interval.getEnd, DurationFieldType.seconds())
    Seconds.seconds(amount)
  }

  def standardSecondsIn(period: ReadablePeriod): Seconds = {
    val amount = BaseSingleFieldPeriod
      .standardPeriodIn(period, DateTimeConstants.MILLIS_PER_SECOND)
    Seconds.seconds(amount)
  }

  @FromString
  def parseSeconds(periodStr: String): Seconds = {
    if (periodStr == null) {
      return Seconds.ZERO
    }
    val p = PARSER.parsePeriod(periodStr)
    Seconds.seconds(p.getSeconds)
  }
}

@SerialVersionUID(87525275727380862L)
class Seconds private (seconds: Int) extends BaseSingleFieldPeriod(seconds) {

  private def readResolve(): AnyRef = Seconds.seconds(getValue)

  def getFieldType(): DurationFieldType = DurationFieldType.seconds()

  def getPeriodType(): PeriodType = PeriodType.seconds()

  def toStandardWeeks(): Weeks = {
    Weeks.weeks(getValue / DateTimeConstants.SECONDS_PER_WEEK)
  }

  def toStandardDays(): Days = {
    Days.days(getValue / DateTimeConstants.SECONDS_PER_DAY)
  }

  def toStandardHours(): Hours = {
    Hours.hours(getValue / DateTimeConstants.SECONDS_PER_HOUR)
  }

  def toStandardMinutes(): Minutes = {
    Minutes.minutes(getValue / DateTimeConstants.SECONDS_PER_MINUTE)
  }

  def toStandardDuration(): Duration = {
    val seconds = getValue
    new Duration(seconds * DateTimeConstants.MILLIS_PER_SECOND)
  }

  def getSeconds(): Int = getValue

  def plus(seconds: Int): Seconds = {
    if (seconds == 0) {
      return this
    }
    Seconds.seconds(FieldUtils.safeAdd(getValue, seconds))
  }

  def plus(seconds: Seconds): Seconds = {
    if (seconds == null) {
      return this
    }
    plus(seconds.getValue)
  }

  def minus(seconds: Int): Seconds = plus(FieldUtils.safeNegate(seconds))

  def minus(seconds: Seconds): Seconds = {
    if (seconds == null) {
      return this
    }
    minus(seconds.getValue)
  }

  def multipliedBy(scalar: Int): Seconds = {
    Seconds.seconds(FieldUtils.safeMultiply(getValue, scalar))
  }

  def dividedBy(divisor: Int): Seconds = {
    if (divisor == 1) {
      return this
    }
    Seconds.seconds(getValue / divisor)
  }

  def negated(): Seconds = {
    Seconds.seconds(FieldUtils.safeNegate(getValue))
  }

  def isGreaterThan(other: Seconds): Boolean = {
    if (other == null) {
      return getValue > 0
    }
    getValue > other.getValue
  }

  def isLessThan(other: Seconds): Boolean = {
    if (other == null) {
      return getValue < 0
    }
    getValue < other.getValue
  }

  @ToString
  override def toString(): String = "PT" + String.valueOf(getValue) + "S"
}
