package org.joda.time

import java.io.Serializable
import org.joda.convert.FromString
import org.joda.time.base.BaseDuration
import org.joda.time.field.FieldUtils

object Duration {

  val ZERO = new Duration(0L)

  @FromString
  def parse(str: String): Duration = new Duration(str)

  def standardDays(days: Long): Duration = {
    if (days == 0) {
      return ZERO
    }
    new Duration(
      FieldUtils.safeMultiply(days, DateTimeConstants.MILLIS_PER_DAY))
  }

  def standardHours(hours: Long): Duration = {
    if (hours == 0) {
      return ZERO
    }
    new Duration(
      FieldUtils.safeMultiply(hours, DateTimeConstants.MILLIS_PER_HOUR))
  }

  def standardMinutes(minutes: Long): Duration = {
    if (minutes == 0) {
      return ZERO
    }
    new Duration(
      FieldUtils.safeMultiply(minutes, DateTimeConstants.MILLIS_PER_MINUTE))
  }

  def standardSeconds(seconds: Long): Duration = {
    if (seconds == 0) {
      return ZERO
    }
    new Duration(
      FieldUtils.safeMultiply(seconds, DateTimeConstants.MILLIS_PER_SECOND))
  }

  def millis(millis: Long): Duration = {
    if (millis == 0) {
      return ZERO
    }
    new Duration(millis)
  }
}

@SerialVersionUID(2471658376918L)
class Duration()
    extends BaseDuration(DummyImplicit)
    with ReadableDuration
    with Serializable {

  def this(duration: Long) {
    this()
    super.auxConstructor(duration)
  }

  def this(startInstant: Long, endInstant: Long) {
    this()
    super.auxConstructor(startInstant, endInstant)
  }

  def this(start: ReadableInstant, end: ReadableInstant) {
    this()
    super.auxConstructor(start, end)
  }

  def this(duration: AnyRef) {
    this()
    super.auxConstructor(duration)
  }

  def getStandardDays(): Long = {
    getMillis / DateTimeConstants.MILLIS_PER_DAY
  }

  def getStandardHours(): Long = {
    getMillis / DateTimeConstants.MILLIS_PER_HOUR
  }

  def getStandardMinutes(): Long = {
    getMillis / DateTimeConstants.MILLIS_PER_MINUTE
  }

  def getStandardSeconds(): Long = {
    getMillis / DateTimeConstants.MILLIS_PER_SECOND
  }

  override def toDuration(): Duration = this

  def toStandardDays(): Days = {
    val days = getStandardDays
    Days.days(FieldUtils.safeToInt(days))
  }

  def toStandardHours(): Hours = {
    val hours = getStandardHours
    Hours.hours(FieldUtils.safeToInt(hours))
  }

  def toStandardMinutes(): Minutes = {
    val minutes = getStandardMinutes
    Minutes.minutes(FieldUtils.safeToInt(minutes))
  }

  def toStandardSeconds(): Seconds = {
    val seconds = getStandardSeconds
    Seconds.seconds(FieldUtils.safeToInt(seconds))
  }

  def withMillis(duration: Long): Duration = {
    if (duration == getMillis) {
      return this
    }
    new Duration(duration)
  }

  def withDurationAdded(durationToAdd: Long, scalar: Int): Duration = {
    if (durationToAdd == 0 || scalar == 0) {
      return this
    }
    val add = FieldUtils.safeMultiply(durationToAdd, scalar)
    val duration = FieldUtils.safeAdd(getMillis, add)
    new Duration(duration)
  }

  def withDurationAdded(durationToAdd: ReadableDuration,
                        scalar: Int): Duration = {
    if (durationToAdd == null || scalar == 0) {
      return this
    }
    withDurationAdded(durationToAdd.getMillis, scalar)
  }

  def plus(amount: Long): Duration = withDurationAdded(amount, 1)

  def plus(amount: ReadableDuration): Duration = {
    if (amount == null) {
      return this
    }
    withDurationAdded(amount.getMillis, 1)
  }

  def minus(amount: Long): Duration = withDurationAdded(amount, -1)

  def minus(amount: ReadableDuration): Duration = {
    if (amount == null) {
      return this
    }
    withDurationAdded(amount.getMillis, -1)
  }

  def multipliedBy(multiplicand: Long): Duration = {
    if (multiplicand == 1) {
      return this
    }
    new Duration(FieldUtils.safeMultiply(getMillis, multiplicand))
  }

  def dividedBy(divisor: Long): Duration = {
    if (divisor == 1) {
      return this
    }
    new Duration(FieldUtils.safeDivide(getMillis, divisor))
  }

  def negated(): Duration = {
    if (getMillis == Long.MinValue) {
      throw new ArithmeticException("Negation of this duration would overflow")
    }
    new Duration(-getMillis)
  }
}
