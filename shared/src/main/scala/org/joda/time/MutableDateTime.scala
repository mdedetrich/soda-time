package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Locale
import org.joda.convert.FromString
import org.joda.time.base.BaseDateTime
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.field.FieldUtils
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import MutableDateTime._

object MutableDateTime {

  val ROUND_NONE = 0
  val ROUND_FLOOR = 1
  val ROUND_CEILING = 2
  val ROUND_HALF_FLOOR = 3
  val ROUND_HALF_CEILING = 4
  val ROUND_HALF_EVEN = 5

  def now(): MutableDateTime = new MutableDateTime()

  def now(zone: DateTimeZone): MutableDateTime = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new MutableDateTime(zone)
  }

  def now(chronology: Chronology): MutableDateTime = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new MutableDateTime(chronology)
  }

  @FromString
  def parse(str: String): MutableDateTime = {
    parse(str, ISODateTimeFormat.dateTimeParser().withOffsetParsed())
  }

  def parse(str: String, formatter: DateTimeFormatter): MutableDateTime = {
    formatter.parseDateTime(str).toMutableDateTime()
  }

  @SerialVersionUID(-4481126543819298617L)
  class Property(private var iInstant: MutableDateTime,
                 private var iField: DateTimeField)
      extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[MutableDateTime]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getMillis

    override protected def getChronology(): Chronology = iInstant.getChronology

    def getMutableDateTime(): MutableDateTime = iInstant

    def add(value: Int): MutableDateTime = {
      iInstant.setMillis(getField.add(iInstant.getMillis, value))
      iInstant
    }

    def add(value: Long): MutableDateTime = {
      iInstant.setMillis(getField.add(iInstant.getMillis, value))
      iInstant
    }

    def addWrapField(value: Int): MutableDateTime = {
      iInstant.setMillis(getField.addWrapField(iInstant.getMillis, value))
      iInstant
    }

    def set(value: Int): MutableDateTime = {
      iInstant.setMillis(getField.set(iInstant.getMillis, value))
      iInstant
    }

    def set(text: String, locale: Locale): MutableDateTime = {
      iInstant.setMillis(getField.set(iInstant.getMillis, text, locale))
      iInstant
    }

    def set(text: String): MutableDateTime = {
      set(text, null)
      iInstant
    }

    def roundFloor(): MutableDateTime = {
      iInstant.setMillis(getField.roundFloor(iInstant.getMillis))
      iInstant
    }

    def roundCeiling(): MutableDateTime = {
      iInstant.setMillis(getField.roundCeiling(iInstant.getMillis))
      iInstant
    }

    def roundHalfFloor(): MutableDateTime = {
      iInstant.setMillis(getField.roundHalfFloor(iInstant.getMillis))
      iInstant
    }

    def roundHalfCeiling(): MutableDateTime = {
      iInstant.setMillis(getField.roundHalfCeiling(iInstant.getMillis))
      iInstant
    }

    def roundHalfEven(): MutableDateTime = {
      iInstant.setMillis(getField.roundHalfEven(iInstant.getMillis))
      iInstant
    }
  }
}

@SerialVersionUID(2852608688135209575L)
class MutableDateTime
    extends BaseDateTime()
    with ReadWritableDateTime
    with Cloneable
    with Serializable {

  private var iRoundingField: DateTimeField = _

  private var iRoundingMode: Int = _

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
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this()
    super.auxConstructor(year,
                         monthOfYear,
                         dayOfMonth,
                         hourOfDay,
                         minuteOfHour,
                         secondOfMinute,
                         millisOfSecond)
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
    super.auxConstructor(year,
                         monthOfYear,
                         dayOfMonth,
                         hourOfDay,
                         minuteOfHour,
                         secondOfMinute,
                         millisOfSecond,
                         zone)
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
    super.auxConstructor(year,
                         monthOfYear,
                         dayOfMonth,
                         hourOfDay,
                         minuteOfHour,
                         secondOfMinute,
                         millisOfSecond,
                         chronology)
  }

  def getRoundingField(): DateTimeField = iRoundingField

  def getRoundingMode(): Int = iRoundingMode

  def setRounding(field: DateTimeField) {
    setRounding(field, MutableDateTime.ROUND_FLOOR)
  }

  def setRounding(field: DateTimeField, mode: Int) {
    if (field != null && (mode < ROUND_NONE || mode > ROUND_HALF_EVEN)) {
      throw new IllegalArgumentException("Illegal rounding mode: " + mode)
    }
    iRoundingField = (if (mode == ROUND_NONE) null else field)
    iRoundingMode = (if (field == null) ROUND_NONE else mode)
    setMillis(getMillis)
  }

  override def setMillis(instant: Long) = {
    val finalVal = iRoundingMode match {
      case ROUND_FLOOR => iRoundingField.roundFloor(instant)
      case ROUND_CEILING => iRoundingField.roundCeiling(instant)
      case ROUND_HALF_FLOOR => iRoundingField.roundHalfFloor(instant)
      case ROUND_HALF_CEILING => iRoundingField.roundHalfCeiling(instant)
      case ROUND_HALF_EVEN => iRoundingField.roundHalfEven(instant)
    }
    super.setMillis(finalVal)
  }

  def setMillis(instant: ReadableInstant) {
    val instantMillis = DateTimeUtils.getInstantMillis(instant)
    setMillis(instantMillis)
  }

  def add(duration: Long) {
    setMillis(FieldUtils.safeAdd(getMillis, duration))
  }

  def add(duration: ReadableDuration) {
    add(duration, 1)
  }

  def add(duration: ReadableDuration, scalar: Int) {
    if (duration != null) {
      add(FieldUtils.safeMultiply(duration.getMillis, scalar))
    }
  }

  def add(period: ReadablePeriod) {
    add(period, 1)
  }

  def add(period: ReadablePeriod, scalar: Int) {
    if (period != null) {
      setMillis(getChronology.add(period, getMillis, scalar))
    }
  }

  override def setChronology(chronology: Chronology) {
    super.setChronology(chronology)
  }

  def setZone(newZone: DateTimeZone) {
    var _newZone = newZone
    _newZone = DateTimeUtils.getZone(_newZone)
    val chrono = getChronology
    if (chrono.getZone != _newZone) {
      setChronology(chrono.withZone(_newZone))
    }
  }

  def setZoneRetainFields(newZone: DateTimeZone) {
    var _newZone = newZone
    _newZone = DateTimeUtils.getZone(_newZone)
    val originalZone = DateTimeUtils.getZone(getZone)
    if (_newZone == originalZone) {
      return
    }
    val millis = originalZone.getMillisKeepLocal(_newZone, getMillis)
    setChronology(getChronology.withZone(_newZone))
    setMillis(millis)
  }

  def set(`type`: DateTimeFieldType, value: Int) {
    if (`type` == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    setMillis(`type`.getField(getChronology).set(getMillis, value))
  }

  def add(`type`: DurationFieldType, amount: Int) {
    if (`type` == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (amount != 0) {
      setMillis(`type`.getField(getChronology).add(getMillis, amount))
    }
  }

  def setYear(year: Int) {
    setMillis(getChronology.year().set(getMillis, year))
  }

  def addYears(years: Int) {
    if (years != 0) {
      setMillis(getChronology.years().add(getMillis, years))
    }
  }

  def setWeekyear(weekyear: Int) {
    setMillis(getChronology.weekyear().set(getMillis, weekyear))
  }

  def addWeekyears(weekyears: Int) {
    if (weekyears != 0) {
      setMillis(getChronology.weekyears().add(getMillis, weekyears))
    }
  }

  def setMonthOfYear(monthOfYear: Int) {
    setMillis(getChronology.monthOfYear().set(getMillis, monthOfYear))
  }

  def addMonths(months: Int) {
    if (months != 0) {
      setMillis(getChronology.months().add(getMillis, months))
    }
  }

  def setWeekOfWeekyear(weekOfWeekyear: Int) {
    setMillis(getChronology.weekOfWeekyear().set(getMillis, weekOfWeekyear))
  }

  def addWeeks(weeks: Int) {
    if (weeks != 0) {
      setMillis(getChronology.weeks().add(getMillis, weeks))
    }
  }

  def setDayOfYear(dayOfYear: Int) {
    setMillis(getChronology.dayOfYear().set(getMillis, dayOfYear))
  }

  def setDayOfMonth(dayOfMonth: Int) {
    setMillis(getChronology.dayOfMonth().set(getMillis, dayOfMonth))
  }

  def setDayOfWeek(dayOfWeek: Int) {
    setMillis(getChronology.dayOfWeek().set(getMillis, dayOfWeek))
  }

  def addDays(days: Int) {
    if (days != 0) {
      setMillis(getChronology.days().add(getMillis, days))
    }
  }

  def setHourOfDay(hourOfDay: Int) {
    setMillis(getChronology.hourOfDay().set(getMillis, hourOfDay))
  }

  def addHours(hours: Int) {
    if (hours != 0) {
      setMillis(getChronology.hours().add(getMillis, hours))
    }
  }

  def setMinuteOfDay(minuteOfDay: Int) {
    setMillis(getChronology.minuteOfDay().set(getMillis, minuteOfDay))
  }

  def setMinuteOfHour(minuteOfHour: Int) {
    setMillis(getChronology.minuteOfHour().set(getMillis, minuteOfHour))
  }

  def addMinutes(minutes: Int) {
    if (minutes != 0) {
      setMillis(getChronology.minutes().add(getMillis, minutes))
    }
  }

  def setSecondOfDay(secondOfDay: Int) {
    setMillis(getChronology.secondOfDay().set(getMillis, secondOfDay))
  }

  def setSecondOfMinute(secondOfMinute: Int) {
    setMillis(getChronology.secondOfMinute().set(getMillis, secondOfMinute))
  }

  def addSeconds(seconds: Int) {
    if (seconds != 0) {
      setMillis(getChronology.seconds().add(getMillis, seconds))
    }
  }

  def setMillisOfDay(millisOfDay: Int) {
    setMillis(getChronology.millisOfDay().set(getMillis, millisOfDay))
  }

  def setMillisOfSecond(millisOfSecond: Int) {
    setMillis(getChronology.millisOfSecond().set(getMillis, millisOfSecond))
  }

  def addMillis(millis: Int) {
    if (millis != 0) {
      setMillis(getChronology.millis().add(getMillis, millis))
    }
  }

  def setDate(instant: Long) {
    setMillis(getChronology.millisOfDay().set(instant, getMillisOfDay))
  }

  def setDate(instant: ReadableInstant) {
    var instantMillis = DateTimeUtils.getInstantMillis(instant)
    if (instant.isInstanceOf[ReadableDateTime]) {
      val rdt = instant.asInstanceOf[ReadableDateTime]
      val instantChrono = DateTimeUtils.getChronology(rdt.getChronology)
      val zone = instantChrono.getZone
      if (zone != null) {
        instantMillis = zone.getMillisKeepLocal(getZone, instantMillis)
      }
    }
    setDate(instantMillis)
  }

  def setDate(year: Int, monthOfYear: Int, dayOfMonth: Int) {
    val c = getChronology
    val instantMidnight = c.getDateTimeMillis(year, monthOfYear, dayOfMonth, 0)
    setDate(instantMidnight)
  }

  def setTime(millis: Long) {
    val millisOfDay = ISOChronology.getInstanceUTC.millisOfDay().get(millis)
    setMillis(getChronology.millisOfDay().set(getMillis, millisOfDay))
  }

  def setTime(instant: ReadableInstant) {
    var instantMillis = DateTimeUtils.getInstantMillis(instant)
    val instantChrono = DateTimeUtils.getInstantChronology(instant)
    val zone = instantChrono.getZone
    if (zone != null) {
      instantMillis = zone.getMillisKeepLocal(DateTimeZone.UTC, instantMillis)
    }
    setTime(instantMillis)
  }

  def setTime(hour: Int,
              minuteOfHour: Int,
              secondOfMinute: Int,
              millisOfSecond: Int) {
    val instant = getChronology.getDateTimeMillis(getMillis,
                                                  hour,
                                                  minuteOfHour,
                                                  secondOfMinute,
                                                  millisOfSecond)
    setMillis(instant)
  }

  def setDateTime(year: Int,
                  monthOfYear: Int,
                  dayOfMonth: Int,
                  hourOfDay: Int,
                  minuteOfHour: Int,
                  secondOfMinute: Int,
                  millisOfSecond: Int) {
    val instant = getChronology.getDateTimeMillis(year,
                                                  monthOfYear,
                                                  dayOfMonth,
                                                  hourOfDay,
                                                  minuteOfHour,
                                                  secondOfMinute,
                                                  millisOfSecond)
    setMillis(instant)
  }

  def property(`type`: DateTimeFieldType): Property = {
    if (`type` == null) {
      throw new IllegalArgumentException(
        "The DateTimeFieldType must not be null")
    }
    val field = `type`.getField(getChronology)
    if (field.isSupported == false) {
      throw new IllegalArgumentException(
        "Field '" + `type` + "' is not supported")
    }
    new Property(this, field)
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

  def copy(): MutableDateTime = clone().asInstanceOf[MutableDateTime]

  override def clone(): AnyRef = super.clone()
}
