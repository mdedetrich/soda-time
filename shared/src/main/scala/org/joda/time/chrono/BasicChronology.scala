package org.joda.time.chrono

import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeZone
import org.joda.time.DurationFieldType
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.DividedDateTimeField
import org.joda.time.field.FieldUtils
import org.joda.time.field.MillisDurationField
import org.joda.time.field.OffsetDateTimeField
import org.joda.time.field.PreciseDateTimeField
import org.joda.time.field.PreciseDurationField
import org.joda.time.field.RemainderDateTimeField
import org.joda.time.field.ZeroIsMaxDateTimeField
import BasicChronology._

object BasicChronology {

  private val cMillisField = MillisDurationField.INSTANCE
  private val cSecondsField = new PreciseDurationField(
    DurationFieldType.seconds(),
    DateTimeConstants.MILLIS_PER_SECOND)
  private val cMinutesField = new PreciseDurationField(
    DurationFieldType.minutes(),
    DateTimeConstants.MILLIS_PER_MINUTE)
  private val cHoursField = new PreciseDurationField(
    DurationFieldType.hours(),
    DateTimeConstants.MILLIS_PER_HOUR)
  private val cHalfdaysField = new PreciseDurationField(
    DurationFieldType.halfdays(),
    DateTimeConstants.MILLIS_PER_DAY / 2)
  private val cDaysField = new PreciseDurationField(
    DurationFieldType.days(),
    DateTimeConstants.MILLIS_PER_DAY)
  private val cWeeksField = new PreciseDurationField(
    DurationFieldType.weeks(),
    DateTimeConstants.MILLIS_PER_WEEK)
  private val cMillisOfSecondField = new PreciseDateTimeField(
    DateTimeFieldType.millisOfSecond(),
    cMillisField,
    cSecondsField)
  private val cMillisOfDayField = new PreciseDateTimeField(
    DateTimeFieldType.millisOfDay(),
    cMillisField,
    cDaysField)
  private val cSecondOfMinuteField = new PreciseDateTimeField(
    DateTimeFieldType.secondOfMinute(),
    cSecondsField,
    cMinutesField)
  private val cSecondOfDayField = new PreciseDateTimeField(
    DateTimeFieldType.secondOfDay(),
    cSecondsField,
    cDaysField)
  private val cMinuteOfHourField = new PreciseDateTimeField(
    DateTimeFieldType.minuteOfHour(),
    cMinutesField,
    cHoursField)
  private val cMinuteOfDayField = new PreciseDateTimeField(
    DateTimeFieldType.minuteOfDay(),
    cMinutesField,
    cDaysField)
  private val cHourOfDayField = new PreciseDateTimeField(
    DateTimeFieldType.hourOfDay(),
    cHoursField,
    cDaysField)
  private val cHourOfHalfdayField = new PreciseDateTimeField(
    DateTimeFieldType.hourOfHalfday(),
    cHoursField,
    cHalfdaysField)
  private val cClockhourOfDayField = new ZeroIsMaxDateTimeField(
    cHourOfDayField,
    DateTimeFieldType.clockhourOfDay())
  private val cClockhourOfHalfdayField = new ZeroIsMaxDateTimeField(
    cHourOfHalfdayField,
    DateTimeFieldType.clockhourOfHalfday())
  private val cHalfdayOfDayField = new HalfdayField()
  private val CACHE_SIZE = 1 << 10
  private val CACHE_MASK = CACHE_SIZE - 1

  @SerialVersionUID(581601443656929254L)
  private class HalfdayField()
      extends PreciseDateTimeField(DateTimeFieldType.halfdayOfDay(),
                                   cHalfdaysField,
                                   cDaysField) {

    override def getAsText(fieldValue: Int, locale: Locale): String = {
      GJLocaleSymbols.forLocale(locale).halfdayValueToText(fieldValue)
    }

    override def set(millis: Long, text: String, locale: Locale): Long = {
      set(millis, GJLocaleSymbols.forLocale(locale).halfdayTextToValue(text))
    }

    override def getMaximumTextLength(locale: Locale): Int = {
      GJLocaleSymbols.forLocale(locale).getHalfdayMaxTextLength
    }
  }

  private class YearInfo(val iYear: Int, val iFirstDayMillis: Long)
}

@SerialVersionUID(8283225332206808863L)
abstract class BasicChronology(base: Chronology,
                               param: AnyRef,
                               private val minDaysInFirstWeek: Int)
    extends AssembledChronology(base, param) {

  @transient private val iYearInfoCache = new Array[YearInfo](CACHE_SIZE)
  private var iMinDaysInFirstWeek: Int = _

  if (minDaysInFirstWeek < 1 || minDaysInFirstWeek > 7) {
    throw new IllegalArgumentException(
      "Invalid min days in first week: " + minDaysInFirstWeek)
  }

  iMinDaysInFirstWeek = minDaysInFirstWeek

  override def getZone(): DateTimeZone = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getZone
    }
    DateTimeZone.UTC
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 millisOfDay: Int): Long = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
    }
    FieldUtils.verifyValueBounds(DateTimeFieldType.millisOfDay(),
                                 millisOfDay,
                                 0,
                                 DateTimeConstants.MILLIS_PER_DAY - 1)
    getDateMidnightMillis(year, monthOfYear, dayOfMonth) +
      millisOfDay
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getDateTimeMillis(year,
                                    monthOfYear,
                                    dayOfMonth,
                                    hourOfDay,
                                    minuteOfHour,
                                    secondOfMinute,
                                    millisOfSecond)
    }
    FieldUtils
      .verifyValueBounds(DateTimeFieldType.hourOfDay(), hourOfDay, 0, 23)
    FieldUtils
      .verifyValueBounds(DateTimeFieldType.minuteOfHour(), minuteOfHour, 0, 59)
    FieldUtils.verifyValueBounds(DateTimeFieldType.secondOfMinute(),
                                 secondOfMinute,
                                 0,
                                 59)
    FieldUtils.verifyValueBounds(DateTimeFieldType.millisOfSecond(),
                                 millisOfSecond,
                                 0,
                                 999)
    getDateMidnightMillis(year, monthOfYear, dayOfMonth) +
      hourOfDay * DateTimeConstants.MILLIS_PER_HOUR +
      minuteOfHour * DateTimeConstants.MILLIS_PER_MINUTE +
      secondOfMinute * DateTimeConstants.MILLIS_PER_SECOND +
      millisOfSecond
  }

  def getMinimumDaysInFirstWeek(): Int = iMinDaysInFirstWeek

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj != null && getClass == obj.getClass) {
      val chrono = obj.asInstanceOf[BasicChronology]
      return getMinimumDaysInFirstWeek == chrono.getMinimumDaysInFirstWeek &&
        getZone == chrono.getZone
    }
    false
  }

  override def hashCode(): Int = {
    getClass.getName.hashCode * 11 + getZone.hashCode + getMinimumDaysInFirstWeek
  }

  override def toString(): String = {
    val sb = new StringBuilder(60)
    var name = getClass.getName
    val index = name.lastIndexOf('.')
    if (index >= 0) {
      name = name.substring(index + 1)
    }
    sb.append(name)
    sb.append('[')
    val zone = getZone
    if (zone != null) {
      sb.append(zone.getID)
    }
    if (getMinimumDaysInFirstWeek != 4) {
      sb.append(",mdfw=")
      sb.append(getMinimumDaysInFirstWeek)
    }
    sb.append(']')
    sb.toString
  }

  protected def assemble(fields: Fields) {
    fields.millis = cMillisField
    fields.seconds = cSecondsField
    fields.minutes = cMinutesField
    fields.hours = cHoursField
    fields.halfdays = cHalfdaysField
    fields.days = cDaysField
    fields.weeks = cWeeksField
    fields.millisOfSecond = cMillisOfSecondField
    fields.millisOfDay = cMillisOfDayField
    fields.secondOfMinute = cSecondOfMinuteField
    fields.secondOfDay = cSecondOfDayField
    fields.minuteOfHour = cMinuteOfHourField
    fields.minuteOfDay = cMinuteOfDayField
    fields.hourOfDay = cHourOfDayField
    fields.hourOfHalfday = cHourOfHalfdayField
    fields.clockhourOfDay = cClockhourOfDayField
    fields.clockhourOfHalfday = cClockhourOfHalfdayField
    fields.halfdayOfDay = cHalfdayOfDayField
    fields.year = new BasicYearDateTimeField(this)
    fields.yearOfEra = new GJYearOfEraDateTimeField(fields.year, this)
    var field: DateTimeField = new OffsetDateTimeField(fields.yearOfEra, 99)
    fields.centuryOfEra =
      new DividedDateTimeField(field, DateTimeFieldType.centuryOfEra(), 100)
    fields.centuries = fields.centuryOfEra.getDurationField
    field = new RemainderDateTimeField(
      fields.centuryOfEra.asInstanceOf[DividedDateTimeField])
    fields.yearOfCentury =
      new OffsetDateTimeField(field, DateTimeFieldType.yearOfCentury(), 1)
    fields.era = new GJEraDateTimeField(this)
    fields.dayOfWeek = new GJDayOfWeekDateTimeField(this, fields.days)
    fields.dayOfMonth = new BasicDayOfMonthDateTimeField(this, fields.days)
    fields.dayOfYear = new BasicDayOfYearDateTimeField(this, fields.days)
    fields.monthOfYear = new GJMonthOfYearDateTimeField(this)
    fields.weekyear = new BasicWeekyearDateTimeField(this)
    fields.weekOfWeekyear =
      new BasicWeekOfWeekyearDateTimeField(this, fields.weeks)
    field = new RemainderDateTimeField(fields.weekyear,
                                       fields.centuries,
                                       DateTimeFieldType.weekyearOfCentury(),
                                       100)
    fields.weekyearOfCentury =
      new OffsetDateTimeField(field, DateTimeFieldType.weekyearOfCentury(), 1)
    fields.years = fields.year.getDurationField
    fields.months = fields.monthOfYear.getDurationField
    fields.weekyears = fields.weekyear.getDurationField
  }

  def getDaysInYearMax(): Int = 366

  def getDaysInYear(year: Int): Int = if (isLeapYear(year)) 366 else 365

  def getWeeksInYear(year: Int): Int = {
    val firstWeekMillis1 = getFirstWeekOfYearMillis(year)
    val firstWeekMillis2 = getFirstWeekOfYearMillis(year + 1)
    ((firstWeekMillis2 - firstWeekMillis1) / DateTimeConstants.MILLIS_PER_WEEK).toInt
  }

  def getFirstWeekOfYearMillis(year: Int): Long = {
    val jan1millis = getYearMillis(year)
    val jan1dayOfWeek = getDayOfWeek(jan1millis)
    if (jan1dayOfWeek > (8 - iMinDaysInFirstWeek)) {
      jan1millis +
        (8 - jan1dayOfWeek) * DateTimeConstants.MILLIS_PER_DAY.toLong
    } else {
      jan1millis -
        (jan1dayOfWeek - 1) * DateTimeConstants.MILLIS_PER_DAY.toLong
    }
  }

  def getYearMillis(year: Int): Long = getYearInfo(year).iFirstDayMillis

  def getYearMonthMillis(year: Int, month: Int): Long = {
    var millis = getYearMillis(year)
    millis += getTotalMillisByYearMonth(year, month)
    millis
  }

  def getYearMonthDayMillis(year: Int, month: Int, dayOfMonth: Int): Long = {
    var millis = getYearMillis(year)
    millis += getTotalMillisByYearMonth(year, month)
    millis +
      (dayOfMonth - 1) * DateTimeConstants.MILLIS_PER_DAY.toLong
  }

  def getYear(instant: Long): Int = {
    val unitMillis = getAverageMillisPerYearDividedByTwo
    var i2 = (instant >> 1) + getApproxMillisAtEpochDividedByTwo
    if (i2 < 0) {
      i2 = i2 - unitMillis + 1
    }
    var year = (i2 / unitMillis).toInt
    var yearStart = getYearMillis(year)
    val diff = instant - yearStart
    if (diff < 0) {
      year -= 1
    } else if (diff >= DateTimeConstants.MILLIS_PER_DAY * 365L) {
      var oneYear: Long = 0l
      oneYear =
        if (isLeapYear(year)) DateTimeConstants.MILLIS_PER_DAY * 366L
        else DateTimeConstants.MILLIS_PER_DAY * 365L
      yearStart += oneYear
      if (yearStart <= instant) {
        year += 1
      }
    }
    year
  }

  def getMonthOfYear(millis: Long): Int =
    getMonthOfYear(millis, getYear(millis))

  def getMonthOfYear(millis: Long, year: Int): Int

  def getDayOfMonth(millis: Long): Int = {
    val year = getYear(millis)
    val month = getMonthOfYear(millis, year)
    getDayOfMonth(millis, year, month)
  }

  def getDayOfMonth(millis: Long, year: Int): Int = {
    val month = getMonthOfYear(millis, year)
    getDayOfMonth(millis, year, month)
  }

  def getDayOfMonth(millis: Long, year: Int, month: Int): Int = {
    var dateMillis = getYearMillis(year)
    dateMillis += getTotalMillisByYearMonth(year, month)
    ((millis - dateMillis) / DateTimeConstants.MILLIS_PER_DAY).toInt +
      1
  }

  def getDayOfYear(instant: Long): Int =
    getDayOfYear(instant, getYear(instant))

  def getDayOfYear(instant: Long, year: Int): Int = {
    val yearStart = getYearMillis(year)
    ((instant - yearStart) / DateTimeConstants.MILLIS_PER_DAY).toInt +
      1
  }

  def getWeekyear(instant: Long): Int = {
    val year = getYear(instant)
    val week = getWeekOfWeekyear(instant, year)
    if (week == 1) {
      getYear(instant + DateTimeConstants.MILLIS_PER_WEEK)
    } else if (week > 51) {
      getYear(instant - (2 * DateTimeConstants.MILLIS_PER_WEEK))
    } else {
      year
    }
  }

  def getWeekOfWeekyear(instant: Long): Int = {
    getWeekOfWeekyear(instant, getYear(instant))
  }

  def getWeekOfWeekyear(instant: Long, year: Int): Int = {
    val firstWeekMillis1 = getFirstWeekOfYearMillis(year)
    if (instant < firstWeekMillis1) {
      return getWeeksInYear(year - 1)
    }
    val firstWeekMillis2 = getFirstWeekOfYearMillis(year + 1)
    if (instant >= firstWeekMillis2) {
      return 1
    }
    ((instant - firstWeekMillis1) / DateTimeConstants.MILLIS_PER_WEEK).toInt +
      1
  }

  def getDayOfWeek(instant: Long): Int = {
    var daysSince19700101: Long = 0l
    if (instant >= 0) {
      daysSince19700101 = instant / DateTimeConstants.MILLIS_PER_DAY
    } else {
      daysSince19700101 = (instant - (DateTimeConstants.MILLIS_PER_DAY - 1)) / DateTimeConstants.MILLIS_PER_DAY
      if (daysSince19700101 < -3) {
        return 7 + ((daysSince19700101 + 4) % 7).toInt
      }
    }
    1 + ((daysSince19700101 + 3) % 7).toInt
  }

  def getMillisOfDay(instant: Long): Int = {
    if (instant >= 0) {
      (instant % DateTimeConstants.MILLIS_PER_DAY).toInt
    } else {
      (DateTimeConstants.MILLIS_PER_DAY - 1) +
        ((instant + 1) % DateTimeConstants.MILLIS_PER_DAY).toInt
    }
  }

  def getDaysInMonthMax(): Int = 31

  def getDaysInMonthMax(instant: Long): Int = {
    val thisYear = getYear(instant)
    val thisMonth = getMonthOfYear(instant, thisYear)
    getDaysInYearMonth(thisYear, thisMonth)
  }

  def getDaysInMonthMaxForSet(instant: Long, value: Int): Int =
    getDaysInMonthMax(instant)

  def getDateMidnightMillis(year: Int,
                            monthOfYear: Int,
                            dayOfMonth: Int): Long = {
    FieldUtils.verifyValueBounds(DateTimeFieldType.year(),
                                 year,
                                 getMinYear,
                                 getMaxYear)
    FieldUtils.verifyValueBounds(DateTimeFieldType.monthOfYear(),
                                 monthOfYear,
                                 1,
                                 getMaxMonth(year))
    FieldUtils.verifyValueBounds(DateTimeFieldType.dayOfMonth(),
                                 dayOfMonth,
                                 1,
                                 getDaysInYearMonth(year, monthOfYear))
    getYearMonthDayMillis(year, monthOfYear, dayOfMonth)
  }

  def getYearDifference(minuendInstant: Long, subtrahendInstant: Long): Long

  def isLeapYear(year: Int): Boolean

  def isLeapDay(instant: Long): Boolean = false

  def getDaysInYearMonth(year: Int, month: Int): Int

  def getDaysInMonthMax(month: Int): Int

  def getTotalMillisByYearMonth(year: Int, month: Int): Long

  def calculateFirstDayOfYearMillis(year: Int): Long

  def getMinYear(): Int

  def getMaxYear(): Int

  def getMaxMonth(year: Int): Int = getMaxMonth

  def getMaxMonth(): Int = 12

  def getAverageMillisPerYear(): Long

  def getAverageMillisPerYearDividedByTwo(): Long

  def getAverageMillisPerMonth(): Long

  def getApproxMillisAtEpochDividedByTwo(): Long

  def setYear(instant: Long, year: Int): Long

  private def getYearInfo(year: Int): YearInfo = {
    var info = iYearInfoCache(year & CACHE_MASK)
    if (info == null || info.iYear != year) {
      info = new YearInfo(year, calculateFirstDayOfYearMillis(year))
      iYearInfoCache(year & CACHE_MASK) = info
    }
    info
  }
}
