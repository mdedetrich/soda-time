package org.joda.time

import java.io.Serializable
import org.joda.convert.FromString
import org.joda.time.base.BasePeriod
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat
import org.joda.time.format.PeriodFormatter
import Period._

object Period {

  val ZERO = new Period()

  @FromString
  def parse(str: String): Period = parse(str, ISOPeriodFormat.standard())

  def parse(str: String, formatter: PeriodFormatter): Period =
    formatter.parsePeriod(str)

  def years(years: Int): Period = {
    new Period(Array(years, 0, 0, 0, 0, 0, 0, 0, 0), PeriodType.standard())
  }

  def months(months: Int): Period = {
    new Period(Array(0, months, 0, 0, 0, 0, 0, 0), PeriodType.standard())
  }

  def weeks(weeks: Int): Period = {
    new Period(Array(0, 0, weeks, 0, 0, 0, 0, 0), PeriodType.standard())
  }

  def days(days: Int): Period = {
    new Period(Array(0, 0, 0, days, 0, 0, 0, 0), PeriodType.standard())
  }

  def hours(hours: Int): Period = {
    new Period(Array(0, 0, 0, 0, hours, 0, 0, 0), PeriodType.standard())
  }

  def minutes(minutes: Int): Period = {
    new Period(Array(0, 0, 0, 0, 0, minutes, 0, 0), PeriodType.standard())
  }

  def seconds(seconds: Int): Period = {
    new Period(Array(0, 0, 0, 0, 0, 0, seconds, 0), PeriodType.standard())
  }

  def millis(millis: Int): Period = {
    new Period(Array(0, 0, 0, 0, 0, 0, 0, millis), PeriodType.standard())
  }

  def fieldDifference(start: ReadablePartial, end: ReadablePartial): Period = {
    if (start == null || end == null) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must not be null")
    }
    if (start.size != end.size) {
      throw new IllegalArgumentException(
        "ReadablePartial objects must have the same set of fields")
    }
    val types = Array.ofDim[DurationFieldType](start.size)
    val values = Array.ofDim[Int](start.size)
    for (i <- 0 until start.size()) {
      if (start.getFieldType(i) != end.getFieldType(i)) {
        throw new IllegalArgumentException(
          "ReadablePartial objects must have the same set of fields")
      }
      types(i) = start.getFieldType(i).getDurationType
      if (i > 0 && types(i - 1) == types(i)) {
        throw new IllegalArgumentException(
          "ReadablePartial objects must not have overlapping fields")
      }
      values(i) = end.getValue(i) - start.getValue(i)
    }
    new Period(values, PeriodType.forFields(types))
  }
}

@SerialVersionUID(741052353876488155L)
class Period extends BasePeriod with ReadablePeriod with Serializable {

  super.auxConstructor(0L, null, null)

  def this(hours: Int, minutes: Int, seconds: Int, millis: Int) {
    this()
    super.auxConstructor(0,
                         0,
                         0,
                         0,
                         hours,
                         minutes,
                         seconds,
                         millis,
                         PeriodType.standard())
  }

  def this(years: Int,
           months: Int,
           weeks: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Int,
           millis: Int) {
    this()
    super.auxConstructor(years,
                         months,
                         weeks,
                         days,
                         hours,
                         minutes,
                         seconds,
                         millis,
                         PeriodType.standard())
  }

  def this(years: Int,
           months: Int,
           weeks: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Int,
           millis: Int,
           `type`: PeriodType) {
    this()
    super.auxConstructor(years,
                         months,
                         weeks,
                         days,
                         hours,
                         minutes,
                         seconds,
                         millis,
                         `type`)
  }

  def this(duration: Long) {
    this()
    super.auxConstructor(duration)
  }

  def this(duration: Long, `type`: PeriodType) {
    this()
    super.auxConstructor(duration, `type`, null)
  }

  def this(duration: Long, chronology: Chronology) {
    this()
    super.auxConstructor(duration, null, chronology)
  }

  def this(duration: Long, `type`: PeriodType, chronology: Chronology) {
    this()
    super.auxConstructor(duration, `type`, chronology)
  }

  def this(startInstant: Long, endInstant: Long) {
    this()
    super.auxConstructor(startInstant, endInstant, null, null)
  }

  def this(startInstant: Long, endInstant: Long, `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`, null)
  }

  def this(startInstant: Long, endInstant: Long, chrono: Chronology) {
    this()
    super.auxConstructor(startInstant, endInstant, null, chrono)
  }

  def this(startInstant: Long,
           endInstant: Long,
           `type`: PeriodType,
           chrono: Chronology) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`, chrono)
  }

  def this(startInstant: ReadableInstant, endInstant: ReadableInstant) {
    this()
    super.auxConstructor(startInstant, endInstant, null)
  }

  def this(startInstant: ReadableInstant,
           endInstant: ReadableInstant,
           `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`)
  }

  def this(start: ReadablePartial, end: ReadablePartial) {
    this()
    super.auxConstructor(start, end, null)
  }

  def this(start: ReadablePartial, end: ReadablePartial, `type`: PeriodType) {
    this()
    super.auxConstructor(start, end, `type`)
  }

  def this(startInstant: ReadableInstant, duration: ReadableDuration) {
    this()
    super.auxConstructor(startInstant, duration, null)
  }

  def this(startInstant: ReadableInstant,
           duration: ReadableDuration,
           `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, duration, `type`)
  }

  def this(duration: ReadableDuration, endInstant: ReadableInstant) {
    this()
    super.auxConstructor(duration, endInstant, null)
  }

  def this(duration: ReadableDuration,
           endInstant: ReadableInstant,
           `type`: PeriodType) {
    this()
    super.auxConstructor(duration, endInstant, `type`)
  }

  def this(period: AnyRef) {
    this()
    super.auxConstructor(period, null, null)
  }

  def this(period: AnyRef, `type`: PeriodType) {
    this()
    super.auxConstructor(period, `type`, null)
  }

  def this(period: AnyRef, chrono: Chronology) {
    this()
    super.auxConstructor(period, null, chrono)
  }

  def this(period: AnyRef, `type`: PeriodType, chrono: Chronology) {
    this()
    super.auxConstructor(period, `type`, chrono)
  }

  private def this(values: Array[Int], `type`: PeriodType) {
    this()
    super.auxConstructor(values, `type`)
  }

  override def toPeriod(): Period = this

  def getYears(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.YEAR_INDEX)
  }

  def getMonths(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MONTH_INDEX)
  }

  def getWeeks(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.WEEK_INDEX)
  }

  def getDays(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.DAY_INDEX)
  }

  def getHours(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.HOUR_INDEX)
  }

  def getMinutes(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MINUTE_INDEX)
  }

  def getSeconds(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.SECOND_INDEX)
  }

  def getMillis(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MILLI_INDEX)
  }

  def withPeriodType(`type`: PeriodType): Period = {
    var _type = `type`
    _type = DateTimeUtils.getPeriodType(_type)
    if (_type == getPeriodType) {
      return this
    }
    new Period(this, _type)
  }

  def withFields(period: ReadablePeriod): Period = {
    if (period == null) {
      return this
    }
    var newValues = getValues
    newValues = super.mergePeriodInto(newValues, period)
    new Period(newValues, getPeriodType)
  }

  def withField(field: DurationFieldType, value: Int): Period = {
    if (field == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    val newValues = getValues
    super.setFieldInto(newValues, field, value)
    new Period(newValues, getPeriodType)
  }

  def withFieldAdded(field: DurationFieldType, value: Int): Period = {
    if (field == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (value == 0) {
      return this
    }
    val newValues = getValues
    super.addFieldInto(newValues, field, value)
    new Period(newValues, getPeriodType)
  }

  def withYears(years: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.YEAR_INDEX, values, years)
    new Period(values, getPeriodType)
  }

  def withMonths(months: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.MONTH_INDEX, values, months)
    new Period(values, getPeriodType)
  }

  def withWeeks(weeks: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.WEEK_INDEX, values, weeks)
    new Period(values, getPeriodType)
  }

  def withDays(days: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.DAY_INDEX, values, days)
    new Period(values, getPeriodType)
  }

  def withHours(hours: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.HOUR_INDEX, values, hours)
    new Period(values, getPeriodType)
  }

  def withMinutes(minutes: Int): Period = {
    val values = getValues
    getPeriodType
      .setIndexedField(this, PeriodType.MINUTE_INDEX, values, minutes)
    new Period(values, getPeriodType)
  }

  def withSeconds(seconds: Int): Period = {
    val values = getValues
    getPeriodType
      .setIndexedField(this, PeriodType.SECOND_INDEX, values, seconds)
    new Period(values, getPeriodType)
  }

  def withMillis(millis: Int): Period = {
    val values = getValues
    getPeriodType.setIndexedField(this, PeriodType.MILLI_INDEX, values, millis)
    new Period(values, getPeriodType)
  }

  def plus(period: ReadablePeriod): Period = {
    if (period == null) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this,
                                  PeriodType.YEAR_INDEX,
                                  values,
                                  period.get(DurationFieldType.YEARS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MONTH_INDEX,
                                  values,
                                  period.get(DurationFieldType.MONTHS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.WEEK_INDEX,
                                  values,
                                  period.get(DurationFieldType.WEEKS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.DAY_INDEX,
                                  values,
                                  period.get(DurationFieldType.DAYS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.HOUR_INDEX,
                                  values,
                                  period.get(DurationFieldType.HOURS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MINUTE_INDEX,
                                  values,
                                  period.get(DurationFieldType.MINUTES_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.SECOND_INDEX,
                                  values,
                                  period.get(DurationFieldType.SECONDS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MILLI_INDEX,
                                  values,
                                  period.get(DurationFieldType.MILLIS_TYPE))
    new Period(values, getPeriodType)
  }

  def plusYears(years: Int): Period = {
    if (years == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.YEAR_INDEX, values, years)
    new Period(values, getPeriodType)
  }

  def plusMonths(months: Int): Period = {
    if (months == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.MONTH_INDEX, values, months)
    new Period(values, getPeriodType)
  }

  def plusWeeks(weeks: Int): Period = {
    if (weeks == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.WEEK_INDEX, values, weeks)
    new Period(values, getPeriodType)
  }

  def plusDays(days: Int): Period = {
    if (days == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.DAY_INDEX, values, days)
    new Period(values, getPeriodType)
  }

  def plusHours(hours: Int): Period = {
    if (hours == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.HOUR_INDEX, values, hours)
    new Period(values, getPeriodType)
  }

  def plusMinutes(minutes: Int): Period = {
    if (minutes == 0) {
      return this
    }
    val values = getValues
    getPeriodType
      .addIndexedField(this, PeriodType.MINUTE_INDEX, values, minutes)
    new Period(values, getPeriodType)
  }

  def plusSeconds(seconds: Int): Period = {
    if (seconds == 0) {
      return this
    }
    val values = getValues
    getPeriodType
      .addIndexedField(this, PeriodType.SECOND_INDEX, values, seconds)
    new Period(values, getPeriodType)
  }

  def plusMillis(millis: Int): Period = {
    if (millis == 0) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this, PeriodType.MILLI_INDEX, values, millis)
    new Period(values, getPeriodType)
  }

  def minus(period: ReadablePeriod): Period = {
    if (period == null) {
      return this
    }
    val values = getValues
    getPeriodType.addIndexedField(this,
                                  PeriodType.YEAR_INDEX,
                                  values,
                                  -period.get(DurationFieldType.YEARS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MONTH_INDEX,
                                  values,
                                  -period.get(DurationFieldType.MONTHS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.WEEK_INDEX,
                                  values,
                                  -period.get(DurationFieldType.WEEKS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.DAY_INDEX,
                                  values,
                                  -period.get(DurationFieldType.DAYS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.HOUR_INDEX,
                                  values,
                                  -period.get(DurationFieldType.HOURS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MINUTE_INDEX,
                                  values,
                                  -period.get(DurationFieldType.MINUTES_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.SECOND_INDEX,
                                  values,
                                  -period.get(DurationFieldType.SECONDS_TYPE))
    getPeriodType.addIndexedField(this,
                                  PeriodType.MILLI_INDEX,
                                  values,
                                  -period.get(DurationFieldType.MILLIS_TYPE))
    new Period(values, getPeriodType)
  }

  def minusYears(years: Int): Period = plusYears(-years)

  def minusMonths(months: Int): Period = plusMonths(-months)

  def minusWeeks(weeks: Int): Period = plusWeeks(-weeks)

  def minusDays(days: Int): Period = plusDays(-days)

  def minusHours(hours: Int): Period = plusHours(-hours)

  def minusMinutes(minutes: Int): Period = plusMinutes(-minutes)

  def minusSeconds(seconds: Int): Period = plusSeconds(-seconds)

  def minusMillis(millis: Int): Period = plusMillis(-millis)

  def multipliedBy(scalar: Int): Period = {
    if (this == ZERO || scalar == 1) {
      return this
    }
    val values = getValues
    for (i <- 0 until values.length) {
      values(i) = FieldUtils.safeMultiply(values(i), scalar)
    }
    new Period(values, getPeriodType)
  }

  def negated(): Period = multipliedBy(-1)

  def toStandardWeeks(): Weeks = {
    checkYearsAndMonths("Weeks")
    var millis: Long = getMillis
    millis += getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND
    millis += getMinutes.toLong * DateTimeConstants.MILLIS_PER_MINUTE
    millis += getHours.toLong * DateTimeConstants.MILLIS_PER_HOUR
    millis += getDays.toLong * DateTimeConstants.MILLIS_PER_DAY
    val weeks = getWeeks.toLong + millis / DateTimeConstants.MILLIS_PER_WEEK
    Weeks.weeks(FieldUtils.safeToInt(weeks))
  }

  def toStandardDays(): Days = {
    checkYearsAndMonths("Days")
    var millis: Long = getMillis
    millis += getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND
    millis += getMinutes.toLong * DateTimeConstants.MILLIS_PER_MINUTE
    millis += getHours.toLong * DateTimeConstants.MILLIS_PER_HOUR
    var days: Long = millis / DateTimeConstants.MILLIS_PER_DAY
    days = FieldUtils.safeAdd(days, getDays)
    days = FieldUtils
      .safeAdd(days, getWeeks.toLong * DateTimeConstants.DAYS_PER_WEEK.toLong)
    Days.days(FieldUtils.safeToInt(days))
  }

  def toStandardHours(): Hours = {
    checkYearsAndMonths("Hours")
    var millis: Long = getMillis
    millis += getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND
    millis += getMinutes.toLong * DateTimeConstants.MILLIS_PER_MINUTE
    var hours: Long = millis / DateTimeConstants.MILLIS_PER_HOUR
    hours = FieldUtils.safeAdd(hours, getHours)
    hours = FieldUtils
      .safeAdd(hours, getDays.toLong * DateTimeConstants.HOURS_PER_DAY.toLong)
    hours = FieldUtils.safeAdd(
      hours,
      getWeeks.toLong * DateTimeConstants.HOURS_PER_WEEK.toLong)
    Hours.hours(FieldUtils.safeToInt(hours))
  }

  def toStandardMinutes(): Minutes = {
    checkYearsAndMonths("Minutes")
    var millis: Long = getMillis
    millis += getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND
    var minutes: Long = millis / DateTimeConstants.MILLIS_PER_MINUTE
    minutes = FieldUtils.safeAdd(minutes, getMinutes)
    minutes = FieldUtils.safeAdd(
      minutes,
      getHours.toLong * DateTimeConstants.MINUTES_PER_HOUR.toLong)
    minutes = FieldUtils.safeAdd(
      minutes,
      getDays.toLong * DateTimeConstants.MINUTES_PER_DAY.toLong)
    minutes = FieldUtils.safeAdd(
      minutes,
      getWeeks.toLong * DateTimeConstants.MINUTES_PER_WEEK.toLong)
    Minutes.minutes(FieldUtils.safeToInt(minutes))
  }

  def toStandardSeconds(): Seconds = {
    checkYearsAndMonths("Seconds")
    var seconds: Long = getMillis / DateTimeConstants.MILLIS_PER_SECOND
    seconds = FieldUtils.safeAdd(seconds, getSeconds)
    seconds = FieldUtils.safeAdd(
      seconds,
      getMinutes.toLong * DateTimeConstants.SECONDS_PER_MINUTE.toLong)
    seconds = FieldUtils.safeAdd(
      seconds,
      getHours.toLong * DateTimeConstants.SECONDS_PER_HOUR.toLong)
    seconds = FieldUtils.safeAdd(
      seconds,
      getDays.toLong * DateTimeConstants.SECONDS_PER_DAY.toLong)
    seconds = FieldUtils.safeAdd(
      seconds,
      getWeeks.toLong * DateTimeConstants.SECONDS_PER_WEEK.toLong)
    Seconds.seconds(FieldUtils.safeToInt(seconds))
  }

  def toStandardDuration(): Duration = {
    checkYearsAndMonths("Duration")
    var millis: Long = getMillis
    millis += (getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND.toLong)
    millis += (getMinutes.toLong * DateTimeConstants.MILLIS_PER_MINUTE.toLong)
    millis += (getHours.toLong * DateTimeConstants.MILLIS_PER_HOUR.toLong)
    millis += (getDays.toLong * DateTimeConstants.MILLIS_PER_DAY.toLong)
    millis += (getWeeks.toLong * DateTimeConstants.MILLIS_PER_WEEK.toLong)
    new Duration(millis)
  }

  private def checkYearsAndMonths(destintionType: String) {
    if (getMonths != 0) {
      throw new UnsupportedOperationException(
        "Cannot convert to " + destintionType +
          " as this period contains months and months vary in length")
    }
    if (getYears != 0) {
      throw new UnsupportedOperationException(
        "Cannot convert to " + destintionType +
          " as this period contains years and years vary in length")
    }
  }

  def normalizedStandard(): Period = {
    normalizedStandard(PeriodType.standard())
  }

  def normalizedStandard(`type`: PeriodType): Period = {
    var _type = `type`
    _type = DateTimeUtils.getPeriodType(_type)
    var millis: Long = getMillis
    millis += (getSeconds.toLong * DateTimeConstants.MILLIS_PER_SECOND.toLong)
    millis += (getMinutes.toLong * DateTimeConstants.MILLIS_PER_MINUTE.toLong)
    millis += (getHours.toLong * DateTimeConstants.MILLIS_PER_HOUR.toLong)
    millis += (getDays.toLong * DateTimeConstants.MILLIS_PER_DAY.toLong)
    millis += (getWeeks.toLong * DateTimeConstants.MILLIS_PER_WEEK.toLong)
    var result = new Period(millis, _type, ISOChronology.getInstanceUTC)
    val years = getYears
    val months = getMonths
    if (years != 0 || months != 0) {
      var totalMonths = years * 12L + months
      if (_type.isSupported(DurationFieldType.YEARS_TYPE)) {
        val normalizedYears = FieldUtils.safeToInt(totalMonths / 12)
        result = result.withYears(normalizedYears)
        totalMonths = totalMonths - (normalizedYears * 12)
      }
      if (_type.isSupported(DurationFieldType.MONTHS_TYPE)) {
        val normalizedMonths = FieldUtils.safeToInt(totalMonths)
        result = result.withMonths(normalizedMonths)
        totalMonths = totalMonths - normalizedMonths
      }
      if (totalMonths != 0) {
        throw new UnsupportedOperationException(
          "Unable to normalize as PeriodType is missing either years or months but period has a month/year amount: " +
            toString)
      }
    }
    result
  }
}
