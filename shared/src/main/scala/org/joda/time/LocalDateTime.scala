package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Calendar
import java.util.Date
import java.util.GregorianCalendar
import java.util.Locale
import java.util.TimeZone
import org.joda.convert.{ToString, FromString}
import org.joda.time.base.BaseLocal
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import LocalDateTime._

object LocalDateTime {

  private val YEAR = 0
  private val MONTH_OF_YEAR = 1
  private val DAY_OF_MONTH = 2
  private val MILLIS_OF_DAY = 3

  def now(): LocalDateTime = new LocalDateTime()

  def now(zone: DateTimeZone): LocalDateTime = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new LocalDateTime(zone)
  }

  def now(chronology: Chronology): LocalDateTime = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new LocalDateTime(chronology)
  }

  @FromString
  def parse(str: String): LocalDateTime = {
    parse(str, ISODateTimeFormat.localDateOptionalTimeParser())
  }

  def parse(str: String, formatter: DateTimeFormatter): LocalDateTime = formatter.parseLocalDateTime(str)

  def fromCalendarFields(calendar: Calendar): LocalDateTime = {
    if (calendar == null) {
      throw new IllegalArgumentException("The calendar must not be null")
    }
    val era = calendar.get(Calendar.ERA)
    val yearOfEra = calendar.get(Calendar.YEAR)
    new LocalDateTime(if (era == GregorianCalendar.AD) yearOfEra else 1 - yearOfEra, calendar.get(Calendar.MONTH) + 1,
      calendar.get(Calendar.DAY_OF_MONTH), calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE),
      calendar.get(Calendar.SECOND), calendar.get(Calendar.MILLISECOND))
  }

  def fromDateFields(date: Date): LocalDateTime = {
    if (date == null) {
      throw new IllegalArgumentException("The date must not be null")
    }
    if (date.getTime < 0) {
      val cal = new GregorianCalendar()
      cal.setTime(date)
      return fromCalendarFields(cal)
    }
    new LocalDateTime(date.getYear + 1900, date.getMonth + 1, date.getDate, date.getHours, date.getMinutes,
      date.getSeconds, ((date.getTime % 1000).toInt + 1000) % 1000)
  }

  @SerialVersionUID(-358138762846288L)
  class Property(@transient private var iInstant: LocalDateTime, @transient private var iField: DateTimeField)
    extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[LocalDateTime]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getLocalMillis

    override protected def getChronology(): Chronology = iInstant.getChronology

    def getLocalDateTime(): LocalDateTime = iInstant

    def addToCopy(value: Int): LocalDateTime = {
      iInstant.withLocalMillis(iField.add(iInstant.getLocalMillis, value))
    }

    def addToCopy(value: Long): LocalDateTime = {
      iInstant.withLocalMillis(iField.add(iInstant.getLocalMillis, value))
    }

    def addWrapFieldToCopy(value: Int): LocalDateTime = {
      iInstant.withLocalMillis(iField.addWrapField(iInstant.getLocalMillis, value))
    }

    def setCopy(value: Int): LocalDateTime = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, value))
    }

    def setCopy(text: String, locale: Locale): LocalDateTime = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, text, locale))
    }

    def setCopy(text: String): LocalDateTime = setCopy(text, null)

    def withMaximumValue(): LocalDateTime = setCopy(getMaximumValue)

    def withMinimumValue(): LocalDateTime = setCopy(getMinimumValue)

    def roundFloorCopy(): LocalDateTime = {
      iInstant.withLocalMillis(iField.roundFloor(iInstant.getLocalMillis))
    }

    def roundCeilingCopy(): LocalDateTime = {
      iInstant.withLocalMillis(iField.roundCeiling(iInstant.getLocalMillis))
    }

    def roundHalfFloorCopy(): LocalDateTime = {
      iInstant.withLocalMillis(iField.roundHalfFloor(iInstant.getLocalMillis))
    }

    def roundHalfCeilingCopy(): LocalDateTime = {
      iInstant.withLocalMillis(iField.roundHalfCeiling(iInstant.getLocalMillis))
    }

    def roundHalfEvenCopy(): LocalDateTime = {
      iInstant.withLocalMillis(iField.roundHalfEven(iInstant.getLocalMillis))
    }
  }
}

@SerialVersionUID(-268716875315837168L)
class LocalDateTime(instant: Long, var chronology: Chronology) extends BaseLocal with ReadablePartial with Serializable {

  private var iLocalMillis: Long = _

  private var iChronology = chronology.withUTC()

  chronology = DateTimeUtils.getChronology(chronology)

  private val localMillis = chronology.getZone.getMillisKeepLocal(DateTimeZone.UTC, instant)

  def this() {
    this(DateTimeUtils.currentTimeMillis(), ISOChronology.getInstance)
  }

  def this(zone: DateTimeZone) {
    this(DateTimeUtils.currentTimeMillis(), ISOChronology.getInstance(zone))
  }

  def this(chronology: Chronology) {
    this(DateTimeUtils.currentTimeMillis(), chronology)
  }

  def this(instant: Long) {
    this(instant, ISOChronology.getInstance)
  }

  def this(instant: Long, zone: DateTimeZone) {
    this(instant, ISOChronology.getInstance(zone))
  }



  def this(instant: AnyRef, zone: DateTimeZone) {
    this()
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    var chronology = converter.getChronology(instant, zone)
    chronology = DateTimeUtils.getChronology(chronology)
    iChronology = chronology.withUTC()
    val values = converter.getPartialValues(this, instant, chronology, ISODateTimeFormat.localDateOptionalTimeParser())
    iLocalMillis = iChronology.getDateTimeMillis(values(0), values(1), values(2), values(3))
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    val values = converter.getPartialValues(this, instant, _chronology, ISODateTimeFormat.localDateOptionalTimeParser())
    iLocalMillis = iChronology.getDateTimeMillis(values(0), values(1), values(2), values(3))
  }
  
  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology).withUTC()
    val instant = _chronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond)
    iChronology = _chronology
    iLocalMillis = instant
  }

  def this(instant: AnyRef) {
    this(instant, null.asInstanceOf[Chronology])
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, ISOChronology.getInstanceUTC)
  }


  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int) {
    this(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, 0, ISOChronology.getInstanceUTC)
  }

  private def readResolve(): AnyRef = {
    if (iChronology == null) {
      return new LocalDateTime(iLocalMillis, ISOChronology.getInstanceUTC)
    }
    if (DateTimeZone.UTC == iChronology.getZone == false) {
      return new LocalDateTime(iLocalMillis, iChronology.withUTC())
    }
    this
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int) {
    this(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, 0, 0, ISOChronology.getInstanceUTC)
  }


  def size(): Int = 4

  protected def getField(index: Int, chrono: Chronology): DateTimeField = index match {
    case YEAR => chrono.year()
    case MONTH_OF_YEAR => chrono.monthOfYear()
    case DAY_OF_MONTH => chrono.dayOfMonth()
    case MILLIS_OF_DAY => chrono.millisOfDay()
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  def getValue(index: Int): Int = index match {
    case YEAR => getChronology.year().get(getLocalMillis)
    case MONTH_OF_YEAR => getChronology.monthOfYear().get(getLocalMillis)
    case DAY_OF_MONTH => getChronology.dayOfMonth().get(getLocalMillis)
    case MILLIS_OF_DAY => getChronology.millisOfDay().get(getLocalMillis)
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  override def get(`type`: DateTimeFieldType): Int = {
    if (`type` == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    `type`.getField(getChronology).get(getLocalMillis)
  }

  override def isSupported(`type`: DateTimeFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    `type`.getField(getChronology).isSupported
  }

  def isSupported(`type`: DurationFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    `type`.getField(getChronology).isSupported
  }

  def getLocalMillis(): Long = iLocalMillis

  def getChronology(): Chronology = iChronology

  override def equals(partial: Any): Boolean = {
    if (this == partial) {
      return true
    }
    if (partial.isInstanceOf[LocalDateTime]) {
      val other = partial.asInstanceOf[LocalDateTime]
      if (iChronology == other.iChronology) {
        return iLocalMillis == other.iLocalMillis
      }
    }
    this == partial
  }

  override def compareTo(partial: ReadablePartial): Int = {
    if (this == partial) {
      return 0
    }
    if (partial.isInstanceOf[LocalDateTime]) {
      val other = partial.asInstanceOf[LocalDateTime]
      if (iChronology == other.iChronology) {
        return if (iLocalMillis < other.iLocalMillis) -1 else (if (iLocalMillis == other.iLocalMillis) 0 else 1)
      }
    }
    super.compareTo(partial)
  }

  def toDateTime(): DateTime = {
    toDateTime(null.asInstanceOf[DateTimeZone])
  }

  def toDateTime(zone: DateTimeZone): DateTime = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val chrono = iChronology.withZone(_zone)
    new DateTime(getYear, getMonthOfYear, getDayOfMonth, getHourOfDay, getMinuteOfHour, getSecondOfMinute,
      getMillisOfSecond, chrono)
  }

  def toLocalDate(): LocalDate = {
    new LocalDate(getLocalMillis, getChronology)
  }

  def toLocalTime(): LocalTime = {
    new LocalTime(getLocalMillis, getChronology)
  }

  def toDate(): Date = {
    val dom = getDayOfMonth
    val date = new Date(getYear - 1900, getMonthOfYear - 1, dom, getHourOfDay, getMinuteOfHour, getSecondOfMinute)
    date.setTime(date.getTime + getMillisOfSecond)
    correctDstTransition(date, TimeZone.getDefault)
  }

  def toDate(timeZone: TimeZone): Date = {
    val calendar = Calendar.getInstance(timeZone)
    calendar.clear()
    calendar.set(getYear, getMonthOfYear - 1, getDayOfMonth, getHourOfDay, getMinuteOfHour, getSecondOfMinute)
    val date = calendar.getTime
    date.setTime(date.getTime + getMillisOfSecond)
    correctDstTransition(date, timeZone)
  }

  private def correctDstTransition(date: Date, timeZone: TimeZone): Date = {
    var calendar = Calendar.getInstance(timeZone)
    calendar.setTime(date)
    var check = LocalDateTime.fromCalendarFields(calendar)
    if (check.isBefore(this)) {
      while (check.isBefore(this)) {
        calendar.setTimeInMillis(calendar.getTimeInMillis + 60000)
        check = LocalDateTime.fromCalendarFields(calendar)
      }
      while (check.isBefore(this) == false) {
        calendar.setTimeInMillis(calendar.getTimeInMillis - 1000)
        check = LocalDateTime.fromCalendarFields(calendar)
      }
      calendar.setTimeInMillis(calendar.getTimeInMillis + 1000)
    } else if (check == this) {
      val earlier = Calendar.getInstance(timeZone)
      earlier.setTimeInMillis(calendar.getTimeInMillis - timeZone.getDSTSavings)
      check = LocalDateTime.fromCalendarFields(earlier)
      if (check == this) {
        calendar = earlier
      }
    }
    calendar.getTime
  }

  def withLocalMillis(newMillis: Long): LocalDateTime = {
    (if (newMillis == getLocalMillis) this else new LocalDateTime(newMillis, getChronology))
  }

  def withDate(year: Int, monthOfYear: Int, dayOfMonth: Int): LocalDateTime = {
    val chrono = getChronology
    var instant = getLocalMillis
    instant = chrono.year().set(instant, year)
    instant = chrono.monthOfYear().set(instant, monthOfYear)
    instant = chrono.dayOfMonth().set(instant, dayOfMonth)
    withLocalMillis(instant)
  }

  def withTime(hourOfDay: Int,
               minuteOfHour: Int,
               secondOfMinute: Int,
               millisOfSecond: Int): LocalDateTime = {
    val chrono = getChronology
    var instant = getLocalMillis
    instant = chrono.hourOfDay().set(instant, hourOfDay)
    instant = chrono.minuteOfHour().set(instant, minuteOfHour)
    instant = chrono.secondOfMinute().set(instant, secondOfMinute)
    instant = chrono.millisOfSecond().set(instant, millisOfSecond)
    withLocalMillis(instant)
  }

  def withFields(partial: ReadablePartial): LocalDateTime = {
    if (partial == null) {
      return this
    }
    withLocalMillis(getChronology.set(partial, getLocalMillis))
  }

  def withField(fieldType: DateTimeFieldType, value: Int): LocalDateTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    val instant = fieldType.getField(getChronology).set(getLocalMillis, value)
    withLocalMillis(instant)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): LocalDateTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (amount == 0) {
      return this
    }
    val instant = fieldType.getField(getChronology).add(getLocalMillis, amount)
    withLocalMillis(instant)
  }

  def withDurationAdded(durationToAdd: ReadableDuration, scalar: Int): LocalDateTime = {
    if (durationToAdd == null || scalar == 0) {
      return this
    }
    val instant = getChronology.add(getLocalMillis, durationToAdd.getMillis, scalar)
    withLocalMillis(instant)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): LocalDateTime = {
    if (period == null || scalar == 0) {
      return this
    }
    val instant = getChronology.add(period, getLocalMillis, scalar)
    withLocalMillis(instant)
  }

  def plus(duration: ReadableDuration): LocalDateTime = withDurationAdded(duration, 1)

  def plus(period: ReadablePeriod): LocalDateTime = withPeriodAdded(period, 1)

  def plusYears(years: Int): LocalDateTime = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().add(getLocalMillis, years)
    withLocalMillis(instant)
  }

  def plusMonths(months: Int): LocalDateTime = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().add(getLocalMillis, months)
    withLocalMillis(instant)
  }

  def plusWeeks(weeks: Int): LocalDateTime = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().add(getLocalMillis, weeks)
    withLocalMillis(instant)
  }

  def plusDays(days: Int): LocalDateTime = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().add(getLocalMillis, days)
    withLocalMillis(instant)
  }

  def plusHours(hours: Int): LocalDateTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().add(getLocalMillis, hours)
    withLocalMillis(instant)
  }

  def plusMinutes(minutes: Int): LocalDateTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().add(getLocalMillis, minutes)
    withLocalMillis(instant)
  }

  def plusSeconds(seconds: Int): LocalDateTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().add(getLocalMillis, seconds)
    withLocalMillis(instant)
  }

  def plusMillis(millis: Int): LocalDateTime = {
    if (millis == 0) {
      return this
    }
    val instant = getChronology.millis().add(getLocalMillis, millis)
    withLocalMillis(instant)
  }

  def minus(duration: ReadableDuration): LocalDateTime = withDurationAdded(duration, -1)

  def minus(period: ReadablePeriod): LocalDateTime = withPeriodAdded(period, -1)

  def minusYears(years: Int): LocalDateTime = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().subtract(getLocalMillis, years)
    withLocalMillis(instant)
  }

  def minusMonths(months: Int): LocalDateTime = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().subtract(getLocalMillis, months)
    withLocalMillis(instant)
  }

  def minusWeeks(weeks: Int): LocalDateTime = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().subtract(getLocalMillis, weeks)
    withLocalMillis(instant)
  }

  def minusDays(days: Int): LocalDateTime = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().subtract(getLocalMillis, days)
    withLocalMillis(instant)
  }

  def minusHours(hours: Int): LocalDateTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().subtract(getLocalMillis, hours)
    withLocalMillis(instant)
  }

  def minusMinutes(minutes: Int): LocalDateTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().subtract(getLocalMillis, minutes)
    withLocalMillis(instant)
  }

  def minusSeconds(seconds: Int): LocalDateTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().subtract(getLocalMillis, seconds)
    withLocalMillis(instant)
  }

  def minusMillis(millis: Int): LocalDateTime = {
    if (millis == 0) {
      return this
    }
    val instant = getChronology.millis().subtract(getLocalMillis, millis)
    withLocalMillis(instant)
  }

  def property(fieldType: DateTimeFieldType): Property = {
    if (fieldType == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    if (isSupported(fieldType) == false) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    new Property(this, fieldType.getField(getChronology))
  }

  def getEra(): Int = getChronology.era().get(getLocalMillis)

  def getCenturyOfEra(): Int = {
    getChronology.centuryOfEra().get(getLocalMillis)
  }

  def getYearOfEra(): Int = {
    getChronology.yearOfEra().get(getLocalMillis)
  }

  def getYearOfCentury(): Int = {
    getChronology.yearOfCentury().get(getLocalMillis)
  }

  def getYear(): Int = {
    getChronology.year().get(getLocalMillis)
  }

  def getWeekyear(): Int = {
    getChronology.weekyear().get(getLocalMillis)
  }

  def getMonthOfYear(): Int = {
    getChronology.monthOfYear().get(getLocalMillis)
  }

  def getWeekOfWeekyear(): Int = {
    getChronology.weekOfWeekyear().get(getLocalMillis)
  }

  def getDayOfYear(): Int = {
    getChronology.dayOfYear().get(getLocalMillis)
  }

  def getDayOfMonth(): Int = {
    getChronology.dayOfMonth().get(getLocalMillis)
  }

  def getDayOfWeek(): Int = {
    getChronology.dayOfWeek().get(getLocalMillis)
  }

  def getHourOfDay(): Int = {
    getChronology.hourOfDay().get(getLocalMillis)
  }

  def getMinuteOfHour(): Int = {
    getChronology.minuteOfHour().get(getLocalMillis)
  }

  def getSecondOfMinute(): Int = {
    getChronology.secondOfMinute().get(getLocalMillis)
  }

  def getMillisOfSecond(): Int = {
    getChronology.millisOfSecond().get(getLocalMillis)
  }

  def getMillisOfDay(): Int = {
    getChronology.millisOfDay().get(getLocalMillis)
  }

  def withEra(era: Int): LocalDateTime = {
    withLocalMillis(getChronology.era().set(getLocalMillis, era))
  }

  def withCenturyOfEra(centuryOfEra: Int): LocalDateTime = {
    withLocalMillis(getChronology.centuryOfEra().set(getLocalMillis, centuryOfEra))
  }

  def withYearOfEra(yearOfEra: Int): LocalDateTime = {
    withLocalMillis(getChronology.yearOfEra().set(getLocalMillis, yearOfEra))
  }

  def withYearOfCentury(yearOfCentury: Int): LocalDateTime = {
    withLocalMillis(getChronology.yearOfCentury().set(getLocalMillis, yearOfCentury))
  }

  def withYear(year: Int): LocalDateTime = {
    withLocalMillis(getChronology.year().set(getLocalMillis, year))
  }

  def withWeekyear(weekyear: Int): LocalDateTime = {
    withLocalMillis(getChronology.weekyear().set(getLocalMillis, weekyear))
  }

  def withMonthOfYear(monthOfYear: Int): LocalDateTime = {
    withLocalMillis(getChronology.monthOfYear().set(getLocalMillis, monthOfYear))
  }

  def withWeekOfWeekyear(weekOfWeekyear: Int): LocalDateTime = {
    withLocalMillis(getChronology.weekOfWeekyear().set(getLocalMillis, weekOfWeekyear))
  }

  def withDayOfYear(dayOfYear: Int): LocalDateTime = {
    withLocalMillis(getChronology.dayOfYear().set(getLocalMillis, dayOfYear))
  }

  def withDayOfMonth(dayOfMonth: Int): LocalDateTime = {
    withLocalMillis(getChronology.dayOfMonth().set(getLocalMillis, dayOfMonth))
  }

  def withDayOfWeek(dayOfWeek: Int): LocalDateTime = {
    withLocalMillis(getChronology.dayOfWeek().set(getLocalMillis, dayOfWeek))
  }

  def withHourOfDay(hour: Int): LocalDateTime = {
    withLocalMillis(getChronology.hourOfDay().set(getLocalMillis, hour))
  }

  def withMinuteOfHour(minute: Int): LocalDateTime = {
    withLocalMillis(getChronology.minuteOfHour().set(getLocalMillis, minute))
  }

  def withSecondOfMinute(second: Int): LocalDateTime = {
    withLocalMillis(getChronology.secondOfMinute().set(getLocalMillis, second))
  }

  def withMillisOfSecond(millis: Int): LocalDateTime = {
    withLocalMillis(getChronology.millisOfSecond().set(getLocalMillis, millis))
  }

  def withMillisOfDay(millis: Int): LocalDateTime = {
    withLocalMillis(getChronology.millisOfDay().set(getLocalMillis, millis))
  }

  def era(): Property = new Property(this, getChronology.era())

  def centuryOfEra(): Property = {
    new Property(this, getChronology.centuryOfEra())
  }

  def yearOfCentury(): Property = {
    new Property(this, getChronology.yearOfCentury())
  }

  def yearOfEra(): Property = {
    new Property(this, getChronology.yearOfEra())
  }

  def year(): Property = {
    new Property(this, getChronology.year())
  }

  def weekyear(): Property = {
    new Property(this, getChronology.weekyear())
  }

  def monthOfYear(): Property = {
    new Property(this, getChronology.monthOfYear())
  }

  def weekOfWeekyear(): Property = {
    new Property(this, getChronology.weekOfWeekyear())
  }

  def dayOfYear(): Property = {
    new Property(this, getChronology.dayOfYear())
  }

  def dayOfMonth(): Property = {
    new Property(this, getChronology.dayOfMonth())
  }

  def dayOfWeek(): Property = {
    new Property(this, getChronology.dayOfWeek())
  }

  def hourOfDay(): Property = {
    new Property(this, getChronology.hourOfDay())
  }

  def minuteOfHour(): Property = {
    new Property(this, getChronology.minuteOfHour())
  }

  def secondOfMinute(): Property = {
    new Property(this, getChronology.secondOfMinute())
  }

  def millisOfSecond(): Property = {
    new Property(this, getChronology.millisOfSecond())
  }

  def millisOfDay(): Property = {
    new Property(this, getChronology.millisOfDay())
  }

  @ToString
  override def toString(): String = {
    ISODateTimeFormat.dateTime().print(this)
  }

  def toString(pattern: String): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).print(this)
  }

  def toString(pattern: String, locale: Locale): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).withLocale(locale)
      .print(this)
  }
}
