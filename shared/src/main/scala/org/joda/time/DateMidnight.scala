package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Locale
import org.joda.convert.FromString
import org.joda.time.base.BaseDateTime
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import DateMidnight._

object DateMidnight {

  def now(): DateMidnight = new DateMidnight()

  def now(zone: DateTimeZone): DateMidnight = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new DateMidnight(zone)
  }

  def now(chronology: Chronology): DateMidnight = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new DateMidnight(chronology)
  }

  @FromString
  def parse(str: String): DateMidnight = {
    parse(str, ISODateTimeFormat.dateTimeParser().withOffsetParsed())
  }

  def parse(str: String, formatter: DateTimeFormatter): DateMidnight = {
    formatter.parseDateTime(str).toDateMidnight()
  }

  @SerialVersionUID(257629620L)
  class Property(private var iInstant: DateMidnight, private var iField: DateTimeField)
    extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[DateMidnight]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getMillis

    protected override def getChronology(): Chronology = iInstant.getChronology

    def getDateMidnight(): DateMidnight = iInstant

    def addToCopy(value: Int): DateMidnight = {
      iInstant.withMillis(iField.add(iInstant.getMillis, value))
    }

    def addToCopy(value: Long): DateMidnight = {
      iInstant.withMillis(iField.add(iInstant.getMillis, value))
    }

    def addWrapFieldToCopy(value: Int): DateMidnight = {
      iInstant.withMillis(iField.addWrapField(iInstant.getMillis, value))
    }

    def setCopy(value: Int): DateMidnight = {
      iInstant.withMillis(iField.set(iInstant.getMillis, value))
    }

    def setCopy(text: String, locale: Locale): DateMidnight = {
      iInstant.withMillis(iField.set(iInstant.getMillis, text, locale))
    }

    def setCopy(text: String): DateMidnight = setCopy(text, null)

    def withMaximumValue(): DateMidnight = setCopy(getMaximumValue)

    def withMinimumValue(): DateMidnight = setCopy(getMinimumValue)

    def roundFloorCopy(): DateMidnight = {
      iInstant.withMillis(iField.roundFloor(iInstant.getMillis))
    }

    def roundCeilingCopy(): DateMidnight = {
      iInstant.withMillis(iField.roundCeiling(iInstant.getMillis))
    }

    def roundHalfFloorCopy(): DateMidnight = {
      iInstant.withMillis(iField.roundHalfFloor(iInstant.getMillis))
    }

    def roundHalfCeilingCopy(): DateMidnight = {
      iInstant.withMillis(iField.roundHalfCeiling(iInstant.getMillis))
    }

    def roundHalfEvenCopy(): DateMidnight = {
      iInstant.withMillis(iField.roundHalfEven(iInstant.getMillis))
    }
  }
}

@SerialVersionUID(156371964018738L)
@Deprecated
class DateMidnight extends BaseDateTime with ReadableDateTime with Serializable {
  
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
    super.auxConstructor(zone)
  }

  def this(instant: Long, chronology: Chronology) {
    this()
    super.auxConstructor(instant,chronology)
  }

  def this(instant: AnyRef) {
    this()
    super.auxConstructor(instant)
  }

  def this(instant: AnyRef, zone: DateTimeZone) {
    this()
    super.auxConstructor(instant,zone)
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    super.auxConstructor(instant, DateTimeUtils.getChronology(chronology))
  }

  def this(year: Int, monthOfYear: Int, dayOfMonth: Int) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, 0, 0, 0, 0)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           zone: DateTimeZone) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, 0, 0, 0, 0, zone)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           chronology: Chronology) {
    this()
    super.auxConstructor(year, monthOfYear, dayOfMonth, 0, 0, 0, 0, chronology)
  }

  override protected def checkInstant(instant: Long, chronology: Chronology): Long = {
    chronology.dayOfMonth().roundFloor(instant)
  }

  def withMillis(newMillis: Long): DateMidnight = {
    var _newMillis: Long = newMillis
    val chrono = getChronology
    _newMillis = checkInstant(_newMillis, chrono)
    (if (_newMillis == getMillis) this else new DateMidnight(_newMillis, chrono))
  }

  def withChronology(newChronology: Chronology): DateMidnight = {
    (if (newChronology == getChronology) this else new DateMidnight(getMillis, newChronology))
  }

  def withZoneRetainFields(newZone: DateTimeZone): DateMidnight = {
    var _newZone: DateTimeZone = newZone
    _newZone = DateTimeUtils.getZone(_newZone)
    val originalZone = DateTimeUtils.getZone(getZone)
    if (_newZone == originalZone) {
      return this
    }
    val millis = originalZone.getMillisKeepLocal(_newZone, getMillis)
    new DateMidnight(millis, getChronology.withZone(_newZone))
  }

  def withFields(partial: ReadablePartial): DateMidnight = {
    if (partial == null) {
      return this
    }
    withMillis(getChronology.set(partial, getMillis))
  }

  def withField(fieldType: DateTimeFieldType, value: Int): DateMidnight = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    val instant = fieldType.getField(getChronology).set(getMillis, value)
    withMillis(instant)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): DateMidnight = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (amount == 0) {
      return this
    }
    val instant = fieldType.getField(getChronology).add(getMillis, amount)
    withMillis(instant)
  }

  def withDurationAdded(durationToAdd: Long, scalar: Int): DateMidnight = {
    if (durationToAdd == 0 || scalar == 0) {
      return this
    }
    val instant = getChronology.add(getMillis, durationToAdd, scalar)
    withMillis(instant)
  }

  def withDurationAdded(durationToAdd: ReadableDuration, scalar: Int): DateMidnight = {
    if (durationToAdd == null || scalar == 0) {
      return this
    }
    withDurationAdded(durationToAdd.getMillis, scalar)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): DateMidnight = {
    if (period == null || scalar == 0) {
      return this
    }
    val instant = getChronology.add(period, getMillis, scalar)
    withMillis(instant)
  }

  def plus(duration: Long): DateMidnight = withDurationAdded(duration, 1)

  def plus(duration: ReadableDuration): DateMidnight = withDurationAdded(duration, 1)

  def plus(period: ReadablePeriod): DateMidnight = withPeriodAdded(period, 1)

  def plusYears(years: Int): DateMidnight = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().add(getMillis, years)
    withMillis(instant)
  }

  def plusMonths(months: Int): DateMidnight = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().add(getMillis, months)
    withMillis(instant)
  }

  def plusWeeks(weeks: Int): DateMidnight = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().add(getMillis, weeks)
    withMillis(instant)
  }

  def plusDays(days: Int): DateMidnight = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().add(getMillis, days)
    withMillis(instant)
  }

  def minus(duration: Long): DateMidnight = withDurationAdded(duration, -1)

  def minus(duration: ReadableDuration): DateMidnight = withDurationAdded(duration, -1)

  def minus(period: ReadablePeriod): DateMidnight = withPeriodAdded(period, -1)

  def minusYears(years: Int): DateMidnight = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().subtract(getMillis, years)
    withMillis(instant)
  }

  def minusMonths(months: Int): DateMidnight = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().subtract(getMillis, months)
    withMillis(instant)
  }

  def minusWeeks(weeks: Int): DateMidnight = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().subtract(getMillis, weeks)
    withMillis(instant)
  }

  def minusDays(days: Int): DateMidnight = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().subtract(getMillis, days)
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
  def toYearMonthDay(): YearMonthDay = {
    new YearMonthDay(getMillis, getChronology)
  }

  def toLocalDate(): LocalDate = new LocalDate(getMillis, getChronology)

  def toInterval(): Interval = {
    val chrono = getChronology
    val start = getMillis
    val end = DurationFieldType.days().getField(chrono).add(start, 1)
    new Interval(start, end, chrono)
  }

  def withEra(era: Int): DateMidnight = {
    withMillis(getChronology.era().set(getMillis, era))
  }

  def withCenturyOfEra(centuryOfEra: Int): DateMidnight = {
    withMillis(getChronology.centuryOfEra().set(getMillis, centuryOfEra))
  }

  def withYearOfEra(yearOfEra: Int): DateMidnight = {
    withMillis(getChronology.yearOfEra().set(getMillis, yearOfEra))
  }

  def withYearOfCentury(yearOfCentury: Int): DateMidnight = {
    withMillis(getChronology.yearOfCentury().set(getMillis, yearOfCentury))
  }

  def withYear(year: Int): DateMidnight = {
    withMillis(getChronology.year().set(getMillis, year))
  }

  def withWeekyear(weekyear: Int): DateMidnight = {
    withMillis(getChronology.weekyear().set(getMillis, weekyear))
  }

  def withMonthOfYear(monthOfYear: Int): DateMidnight = {
    withMillis(getChronology.monthOfYear().set(getMillis, monthOfYear))
  }

  def withWeekOfWeekyear(weekOfWeekyear: Int): DateMidnight = {
    withMillis(getChronology.weekOfWeekyear().set(getMillis, weekOfWeekyear))
  }

  def withDayOfYear(dayOfYear: Int): DateMidnight = {
    withMillis(getChronology.dayOfYear().set(getMillis, dayOfYear))
  }

  def withDayOfMonth(dayOfMonth: Int): DateMidnight = {
    withMillis(getChronology.dayOfMonth().set(getMillis, dayOfMonth))
  }

  def withDayOfWeek(dayOfWeek: Int): DateMidnight = {
    withMillis(getChronology.dayOfWeek().set(getMillis, dayOfWeek))
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
}
