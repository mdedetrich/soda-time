package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Locale
import org.joda.convert.FromString
import org.joda.time.base.BaseDateTime
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import DateTime._

object DateTime {

  def now(): DateTime = new DateTime()

  def now(zone: DateTimeZone): DateTime = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new DateTime(zone)
  }

  def now(chronology: Chronology): DateTime = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new DateTime(chronology)
  }

  @FromString
  def parse(str: String): DateTime = {
    parse(str, ISODateTimeFormat.dateTimeParser().withOffsetParsed())
  }

  def parse(str: String, formatter: DateTimeFormatter): DateTime = formatter.parseDateTime(str)

  @SerialVersionUID(-6983323811635733510L)
  class Property(private var iInstant: DateTime, private var iField: DateTimeField)
    extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[DateTime]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getMillis

    override protected def getChronology(): Chronology = iInstant.getChronology

    def getDateTime(): DateTime = iInstant

    def addToCopy(value: Int): DateTime = {
      iInstant.withMillis(iField.add(iInstant.getMillis, value))
    }

    def addToCopy(value: Long): DateTime = {
      iInstant.withMillis(iField.add(iInstant.getMillis, value))
    }

    def addWrapFieldToCopy(value: Int): DateTime = {
      iInstant.withMillis(iField.addWrapField(iInstant.getMillis, value))
    }

    def setCopy(value: Int): DateTime = {
      iInstant.withMillis(iField.set(iInstant.getMillis, value))
    }

    def setCopy(text: String, locale: Locale): DateTime = {
      iInstant.withMillis(iField.set(iInstant.getMillis, text, locale))
    }

    def setCopy(text: String): DateTime = setCopy(text, null)

    def withMaximumValue(): DateTime = {
      try {
        setCopy(getMaximumValue)
      } catch {
        case ex: RuntimeException => {
          if (IllegalInstantException.isIllegalInstant(ex)) {
            val beforeGap = getChronology.getZone.previousTransition(getMillis + DateTimeConstants.MILLIS_PER_DAY)
            return new DateTime(beforeGap, getChronology)
          }
          throw ex
        }
      }
    }

    def withMinimumValue(): DateTime = {
      try {
        setCopy(getMinimumValue)
      } catch {
        case ex: RuntimeException => {
          if (IllegalInstantException.isIllegalInstant(ex)) {
            val afterGap = getChronology.getZone.nextTransition(getMillis - DateTimeConstants.MILLIS_PER_DAY)
            return new DateTime(afterGap, getChronology)
          }
          throw ex
        }
      }
    }

    def roundFloorCopy(): DateTime = {
      iInstant.withMillis(iField.roundFloor(iInstant.getMillis))
    }

    def roundCeilingCopy(): DateTime = {
      iInstant.withMillis(iField.roundCeiling(iInstant.getMillis))
    }

    def roundHalfFloorCopy(): DateTime = {
      iInstant.withMillis(iField.roundHalfFloor(iInstant.getMillis))
    }

    def roundHalfCeilingCopy(): DateTime = {
      iInstant.withMillis(iField.roundHalfCeiling(iInstant.getMillis))
    }

    def roundHalfEvenCopy(): DateTime = {
      iInstant.withMillis(iField.roundHalfEven(iInstant.getMillis))
    }
  }
}

@SerialVersionUID(-5171125899451703815L)
class DateTime extends BaseDateTime with ReadableDateTime with Serializable {

  def this(zone: DateTimeZone) {
    this()
    super.auxConstructor(zone)
  }

  def this(chronology: Chronology) {
    this()
    super.auxConstructor(chronology)
  }

  def this(instant: Long) {
    this()
    super.auxConstructor(instant)
  }

  def this(instant: Long, zone: DateTimeZone) {
    this()
    super.auxConstructor(instant, zone)
  }

  def this(instant: Long, chronology: Chronology) {
    this()
    super.auxConstructor(instant, chronology)
  }

  def this(instant: AnyRef) {
    this()
    super.auxConstructor(instant, null.asInstanceOf[Chronology])
  }

  def this(instant: AnyRef, zone: DateTimeZone) {
    this()
    super.auxConstructor(instant, zone)
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    super.auxConstructor(instant, DateTimeUtils.getChronology(chronology))
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, 0, 0)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           zone: DateTimeZone) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, 0, 0, zone)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           chronology: Chronology) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, 0, 0, chronology)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, 0)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           zone: DateTimeZone) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, 0, zone)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           chronology: Chronology) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, 0, chronology)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           zone: DateTimeZone) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, zone)
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
    super.auxConstructor(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, chronology)
  }

  override def toDateTime(): DateTime = this

  override def toDateTimeISO(): DateTime = {
    if (getChronology == ISOChronology.getInstance) {
      return this
    }
    super.toDateTimeISO()
  }

  override def toDateTime(zone: DateTimeZone): DateTime = {
    var _zone = zone
    _zone = DateTimeUtils.getZone(_zone)
    if (getZone == _zone) {
      return this
    }
    super.toDateTime(_zone)
  }

  override def toDateTime(chronology: Chronology): DateTime = {
    var _chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology)
    if (getChronology == _chronology) {
      return this
    }
    super.toDateTime(_chronology)
  }

  def withMillis(newMillis: Long): DateTime = {
    if (newMillis == getMillis) this else new DateTime(newMillis, getChronology)
  }

  def withChronology(newChronology: Chronology): DateTime = {
    var _newChronology = newChronology
    _newChronology = DateTimeUtils.getChronology(_newChronology)
    if (_newChronology == getChronology) this else new DateTime(getMillis, _newChronology)
  }

  def withZone(newZone: DateTimeZone): DateTime = {
    withChronology(getChronology.withZone(newZone))
  }

  def withZoneRetainFields(newZone: DateTimeZone): DateTime = {
    var _newZone = newZone
    _newZone = DateTimeUtils.getZone(_newZone)
    val originalZone = DateTimeUtils.getZone(getZone)
    if (_newZone == originalZone) {
      return this
    }
    val millis = originalZone.getMillisKeepLocal(_newZone, getMillis)
    new DateTime(millis, getChronology.withZone(_newZone))
  }

  def withEarlierOffsetAtOverlap(): DateTime = {
    val newMillis = getZone.adjustOffset(getMillis, false)
    withMillis(newMillis)
  }

  def withLaterOffsetAtOverlap(): DateTime = {
    val newMillis = getZone.adjustOffset(getMillis, true)
    withMillis(newMillis)
  }

  def withDate(year: Int, monthOfYear: Int, dayOfMonth: Int): DateTime = {
    val chrono = getChronology
    var instant = getMillis
    instant = chrono.year().set(instant, year)
    instant = chrono.monthOfYear().set(instant, monthOfYear)
    instant = chrono.dayOfMonth().set(instant, dayOfMonth)
    withMillis(instant)
  }

  def withDate(date: LocalDate): DateTime = {
    withDate(date.getYear, date.getMonthOfYear, date.getDayOfMonth)
  }

  def withTime(hourOfDay: Int,
               minuteOfHour: Int,
               secondOfMinute: Int,
               millisOfSecond: Int): DateTime = {
    val chrono = getChronology
    var instant = getMillis
    instant = chrono.hourOfDay().set(instant, hourOfDay)
    instant = chrono.minuteOfHour().set(instant, minuteOfHour)
    instant = chrono.secondOfMinute().set(instant, secondOfMinute)
    instant = chrono.millisOfSecond().set(instant, millisOfSecond)
    withMillis(instant)
  }

  def withTime(time: LocalTime): DateTime = {
    withTime(time.getHourOfDay, time.getMinuteOfHour, time.getSecondOfMinute, time.getMillisOfSecond)
  }

  def withTimeAtStartOfDay(): DateTime = {
    toLocalDate().toDateTimeAtStartOfDay(getZone)
  }

  def withFields(partial: ReadablePartial): DateTime = {
    if (partial == null) {
      return this
    }
    withMillis(getChronology.set(partial, getMillis))
  }

  def withField(fieldType: DateTimeFieldType, value: Int): DateTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    val instant = fieldType.getField(getChronology).set(getMillis, value)
    withMillis(instant)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): DateTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (amount == 0) {
      return this
    }
    val instant = fieldType.getField(getChronology).add(getMillis, amount)
    withMillis(instant)
  }

  def withDurationAdded(durationToAdd: Long, scalar: Int): DateTime = {
    if (durationToAdd == 0 || scalar == 0) {
      return this
    }
    val instant = getChronology.add(getMillis, durationToAdd, scalar)
    withMillis(instant)
  }

  def withDurationAdded(durationToAdd: ReadableDuration, scalar: Int): DateTime = {
    if (durationToAdd == null || scalar == 0) {
      return this
    }
    withDurationAdded(durationToAdd.getMillis, scalar)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): DateTime = {
    if (period == null || scalar == 0) {
      return this
    }
    val instant = getChronology.add(period, getMillis, scalar)
    withMillis(instant)
  }

  def plus(duration: Long): DateTime = withDurationAdded(duration, 1)

  def plus(duration: ReadableDuration): DateTime = withDurationAdded(duration, 1)

  def plus(period: ReadablePeriod): DateTime = withPeriodAdded(period, 1)

  def plusYears(years: Int): DateTime = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().add(getMillis, years)
    withMillis(instant)
  }

  def plusMonths(months: Int): DateTime = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().add(getMillis, months)
    withMillis(instant)
  }

  def plusWeeks(weeks: Int): DateTime = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().add(getMillis, weeks)
    withMillis(instant)
  }

  def plusDays(days: Int): DateTime = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().add(getMillis, days)
    withMillis(instant)
  }

  def plusHours(hours: Int): DateTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().add(getMillis, hours)
    withMillis(instant)
  }

  def plusMinutes(minutes: Int): DateTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().add(getMillis, minutes)
    withMillis(instant)
  }

  def plusSeconds(seconds: Int): DateTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().add(getMillis, seconds)
    withMillis(instant)
  }

  def plusMillis(millis: Int): DateTime = {
    if (millis == 0) {
      return this
    }
    val instant = getChronology.millis().add(getMillis, millis)
    withMillis(instant)
  }

  def minus(duration: Long): DateTime = withDurationAdded(duration, -1)

  def minus(duration: ReadableDuration): DateTime = withDurationAdded(duration, -1)

  def minus(period: ReadablePeriod): DateTime = withPeriodAdded(period, -1)

  def minusYears(years: Int): DateTime = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().subtract(getMillis, years)
    withMillis(instant)
  }

  def minusMonths(months: Int): DateTime = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().subtract(getMillis, months)
    withMillis(instant)
  }

  def minusWeeks(weeks: Int): DateTime = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().subtract(getMillis, weeks)
    withMillis(instant)
  }

  def minusDays(days: Int): DateTime = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().subtract(getMillis, days)
    withMillis(instant)
  }

  def minusHours(hours: Int): DateTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().subtract(getMillis, hours)
    withMillis(instant)
  }

  def minusMinutes(minutes: Int): DateTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().subtract(getMillis, minutes)
    withMillis(instant)
  }

  def minusSeconds(seconds: Int): DateTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().subtract(getMillis, seconds)
    withMillis(instant)
  }

  def minusMillis(millis: Int): DateTime = {
    if (millis == 0) {
      return this
    }
    val instant = getChronology.millis().subtract(getMillis, millis)
    withMillis(instant)
  }

  def property(`type`: DateTimeFieldType): Property = {
    if (`type` == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    val field = `type`.getField(getChronology)
    if (field.isSupported == false) {
      throw new IllegalArgumentException("Field '" + `type` + "' is not supported")
    }
    new Property(this, field)
  }

  @Deprecated
  def toDateMidnight(): DateMidnight = {
    new DateMidnight(getMillis, getChronology)
  }

  @Deprecated
  def toYearMonthDay(): YearMonthDay = {
    new YearMonthDay(getMillis, getChronology)
  }

  @Deprecated
  def toTimeOfDay(): TimeOfDay = new TimeOfDay(getMillis, getChronology)

  def toLocalDateTime(): LocalDateTime = {
    new LocalDateTime(getMillis, getChronology)
  }

  def toLocalDate(): LocalDate = new LocalDate(getMillis, getChronology)

  def toLocalTime(): LocalTime = new LocalTime(getMillis, getChronology)

  def withEra(era: Int): DateTime = {
    withMillis(getChronology.era().set(getMillis, era))
  }

  def withCenturyOfEra(centuryOfEra: Int): DateTime = {
    withMillis(getChronology.centuryOfEra().set(getMillis, centuryOfEra))
  }

  def withYearOfEra(yearOfEra: Int): DateTime = {
    withMillis(getChronology.yearOfEra().set(getMillis, yearOfEra))
  }

  def withYearOfCentury(yearOfCentury: Int): DateTime = {
    withMillis(getChronology.yearOfCentury().set(getMillis, yearOfCentury))
  }

  def withYear(year: Int): DateTime = {
    withMillis(getChronology.year().set(getMillis, year))
  }

  def withWeekyear(weekyear: Int): DateTime = {
    withMillis(getChronology.weekyear().set(getMillis, weekyear))
  }

  def withMonthOfYear(monthOfYear: Int): DateTime = {
    withMillis(getChronology.monthOfYear().set(getMillis, monthOfYear))
  }

  def withWeekOfWeekyear(weekOfWeekyear: Int): DateTime = {
    withMillis(getChronology.weekOfWeekyear().set(getMillis, weekOfWeekyear))
  }

  def withDayOfYear(dayOfYear: Int): DateTime = {
    withMillis(getChronology.dayOfYear().set(getMillis, dayOfYear))
  }

  def withDayOfMonth(dayOfMonth: Int): DateTime = {
    withMillis(getChronology.dayOfMonth().set(getMillis, dayOfMonth))
  }

  def withDayOfWeek(dayOfWeek: Int): DateTime = {
    withMillis(getChronology.dayOfWeek().set(getMillis, dayOfWeek))
  }

  def withHourOfDay(hour: Int): DateTime = {
    withMillis(getChronology.hourOfDay().set(getMillis, hour))
  }

  def withMinuteOfHour(minute: Int): DateTime = {
    withMillis(getChronology.minuteOfHour().set(getMillis, minute))
  }

  def withSecondOfMinute(second: Int): DateTime = {
    withMillis(getChronology.secondOfMinute().set(getMillis, second))
  }

  def withMillisOfSecond(millis: Int): DateTime = {
    withMillis(getChronology.millisOfSecond().set(getMillis, millis))
  }

  def withMillisOfDay(millis: Int): DateTime = {
    withMillis(getChronology.millisOfDay().set(getMillis, millis))
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

  def minuteOfDay(): Property = {
    new Property(this, getChronology.minuteOfDay())
  }

  def minuteOfHour(): Property = {
    new Property(this, getChronology.minuteOfHour())
  }

  def secondOfDay(): Property = {
    new Property(this, getChronology.secondOfDay())
  }

  def secondOfMinute(): Property = {
    new Property(this, getChronology.secondOfMinute())
  }

  def millisOfDay(): Property = {
    new Property(this, getChronology.millisOfDay())
  }

  def millisOfSecond(): Property = {
    new Property(this, getChronology.millisOfSecond())
  }
}
