package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Calendar
import java.util.Date
import java.util.Locale
import org.joda.convert.{ToString, FromString}
import org.joda.time.base.BaseLocal
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import LocalTime._

object LocalTime {

  val MIDNIGHT = new LocalTime(0, 0, 0, 0)

  private val HOUR_OF_DAY = 0
  private val MINUTE_OF_HOUR = 1
  private val SECOND_OF_MINUTE = 2
  private val MILLIS_OF_SECOND = 3
  private val TIME_DURATION_TYPES = new collection.mutable.HashSet[DurationFieldType]()

  TIME_DURATION_TYPES.add(DurationFieldType.millis())
  TIME_DURATION_TYPES.add(DurationFieldType.seconds())
  TIME_DURATION_TYPES.add(DurationFieldType.minutes())
  TIME_DURATION_TYPES.add(DurationFieldType.hours())

  def now(): LocalTime = new LocalTime()

  def now(zone: DateTimeZone): LocalTime = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new LocalTime(zone)
  }

  def now(chronology: Chronology): LocalTime = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new LocalTime(chronology)
  }

  @FromString
  def parse(str: String): LocalTime = {
    parse(str, ISODateTimeFormat.localTimeParser())
  }

  def parse(str: String, formatter: DateTimeFormatter): LocalTime = formatter.parseLocalTime(str)

  def fromMillisOfDay(millisOfDay: Long): LocalTime = fromMillisOfDay(millisOfDay, null)

  def fromMillisOfDay(millisOfDay: Long, chrono: Chronology): LocalTime = {
    var _chrono: Chronology = chrono
    _chrono = DateTimeUtils.getChronology(_chrono).withUTC()
    new LocalTime(millisOfDay, _chrono)
  }

  def fromCalendarFields(calendar: Calendar): LocalTime = {
    if (calendar == null) {
      throw new IllegalArgumentException("The calendar must not be null")
    }
    new LocalTime(calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE), calendar.get(Calendar.SECOND),
      calendar.get(Calendar.MILLISECOND))
  }

  def fromDateFields(date: Date): LocalTime = {
    if (date == null) {
      throw new IllegalArgumentException("The date must not be null")
    }
    new LocalTime(date.getHours, date.getMinutes, date.getSeconds, ((date.getTime % 1000).toInt + 1000) % 1000)
  }

  @SerialVersionUID(-325842547277223L)
  class Property(@transient private var iInstant: LocalTime, @transient private var iField: DateTimeField)
    extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[LocalTime]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getLocalMillis

    override protected def getChronology(): Chronology = iInstant.getChronology

    def getLocalTime(): LocalTime = iInstant

    def addCopy(value: Int): LocalTime = {
      iInstant.withLocalMillis(iField.add(iInstant.getLocalMillis, value))
    }

    def addCopy(value: Long): LocalTime = {
      iInstant.withLocalMillis(iField.add(iInstant.getLocalMillis, value))
    }

    def addNoWrapToCopy(value: Int): LocalTime = {
      val millis = iField.add(iInstant.getLocalMillis, value)
      val rounded = iInstant.getChronology.millisOfDay().get(millis)
      if (rounded != millis) {
        throw new IllegalArgumentException("The addition exceeded the boundaries of LocalTime")
      }
      iInstant.withLocalMillis(millis)
    }

    def addWrapFieldToCopy(value: Int): LocalTime = {
      iInstant.withLocalMillis(iField.addWrapField(iInstant.getLocalMillis, value))
    }

    def setCopy(value: Int): LocalTime = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, value))
    }

    def setCopy(text: String, locale: Locale): LocalTime = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, text, locale))
    }

    def setCopy(text: String): LocalTime = setCopy(text, null)

    def withMaximumValue(): LocalTime = setCopy(getMaximumValue)

    def withMinimumValue(): LocalTime = setCopy(getMinimumValue)

    def roundFloorCopy(): LocalTime = {
      iInstant.withLocalMillis(iField.roundFloor(iInstant.getLocalMillis))
    }

    def roundCeilingCopy(): LocalTime = {
      iInstant.withLocalMillis(iField.roundCeiling(iInstant.getLocalMillis))
    }

    def roundHalfFloorCopy(): LocalTime = {
      iInstant.withLocalMillis(iField.roundHalfFloor(iInstant.getLocalMillis))
    }

    def roundHalfCeilingCopy(): LocalTime = {
      iInstant.withLocalMillis(iField.roundHalfCeiling(iInstant.getLocalMillis))
    }

    def roundHalfEvenCopy(): LocalTime = {
      iInstant.withLocalMillis(iField.roundHalfEven(iInstant.getLocalMillis))
    }
  }
}

@SerialVersionUID(-12873158713873L)
class LocalTime(instant: Long, private var chronology: Chronology) extends BaseLocal with ReadablePartial with Serializable {

  val localMillis = chronology.getZone.getMillisKeepLocal(DateTimeZone.UTC, instant)

  private var iChronology: Chronology = null
  private var iLocalMillis: Long = chronology.millisOfDay().get(localMillis)

  chronology = DateTimeUtils.getChronology(chronology)

  chronology = chronology.withUTC()

  iLocalMillis = chronology.millisOfDay.get(localMillis)
  iChronology = chronology

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
    val values = converter.getPartialValues(this, instant, chronology, ISODateTimeFormat.localTimeParser())
    iLocalMillis = iChronology.getDateTimeMillis(0L, values(0), values(1), values(2), values(3))
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    val values = converter.getPartialValues(this, instant, _chronology, ISODateTimeFormat.localTimeParser())
    iLocalMillis = iChronology.getDateTimeMillis(0L, values(0), values(1), values(2), values(3))
  }

  def this(instant: AnyRef) {
    this(instant, null.asInstanceOf[Chronology])
  }

  def this(hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology).withUTC()
    val instant = _chronology.getDateTimeMillis(0L, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond)
    iChronology = _chronology
    iLocalMillis = instant
  }

  def this(hourOfDay: Int, minuteOfHour: Int) {
    this(hourOfDay, minuteOfHour, 0, 0, ISOChronology.getInstanceUTC)
  }

  def this(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int) {
    this(hourOfDay, minuteOfHour, secondOfMinute, 0, ISOChronology.getInstanceUTC)
  }

  def this(hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this(hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, ISOChronology.getInstanceUTC)
  }

  private def readResolve(): AnyRef = {
    if (iChronology == null) {
      return new LocalTime(iLocalMillis, ISOChronology.getInstanceUTC)
    }
    if (!(DateTimeZone.UTC == iChronology.getZone)) {
      return new LocalTime(iLocalMillis, iChronology.withUTC())
    }
    this
  }

  def size(): Int = 4

  protected def getField(index: Int, chrono: Chronology): DateTimeField = index match {
    case HOUR_OF_DAY => chrono.hourOfDay()
    case MINUTE_OF_HOUR => chrono.minuteOfHour()
    case SECOND_OF_MINUTE => chrono.secondOfMinute()
    case MILLIS_OF_SECOND => chrono.millisOfSecond()
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  def getValue(index: Int): Int = index match {
    case HOUR_OF_DAY => getChronology.hourOfDay().get(getLocalMillis)
    case MINUTE_OF_HOUR => getChronology.minuteOfHour().get(getLocalMillis)
    case SECOND_OF_MINUTE => getChronology.secondOfMinute().get(getLocalMillis)
    case MILLIS_OF_SECOND => getChronology.millisOfSecond().get(getLocalMillis)
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  override def get(fieldType: DateTimeFieldType): Int = {
    if (fieldType == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    if (!isSupported(fieldType)) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    fieldType.getField(getChronology).get(getLocalMillis)
  }

  override def isSupported(`type`: DateTimeFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    if (isSupported(`type`.getDurationType) == false) {
      return false
    }
    val range = `type`.getRangeDurationType
    isSupported(range) || range == DurationFieldType.days()
  }

  def isSupported(`type`: DurationFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    val field = `type`.getField(getChronology)
    if (TIME_DURATION_TYPES.contains(`type`) ||
      field.getUnitMillis < getChronology.days().getUnitMillis) {
      return field.isSupported
    }
    false
  }

  def getLocalMillis(): Long = iLocalMillis

  def getChronology(): Chronology = iChronology

  override def equals(partial: Any): Boolean = {
    if (this == partial) {
      return true
    }
    if (partial.isInstanceOf[LocalTime]) {
      val other = partial.asInstanceOf[LocalTime]
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
    if (partial.isInstanceOf[LocalTime]) {
      val other = partial.asInstanceOf[LocalTime]
      if (iChronology == other.iChronology) {
        return if (iLocalMillis < other.iLocalMillis) -1 else if (iLocalMillis == other.iLocalMillis) 0 else 1
      }
    }
    super.compareTo(partial)
  }

  def withLocalMillis(newMillis: Long): LocalTime = {
    if (newMillis == getLocalMillis) this else new LocalTime(newMillis, getChronology)
  }

  def withFields(partial: ReadablePartial): LocalTime = {
    if (partial == null) {
      return this
    }
    withLocalMillis(getChronology.set(partial, getLocalMillis))
  }

  def withField(fieldType: DateTimeFieldType, value: Int): LocalTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (!isSupported(fieldType)) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    val instant = fieldType.getField(getChronology).set(getLocalMillis, value)
    withLocalMillis(instant)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): LocalTime = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (!isSupported(fieldType)) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    if (amount == 0) {
      return this
    }
    val instant = fieldType.getField(getChronology).add(getLocalMillis, amount)
    withLocalMillis(instant)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): LocalTime = {
    if (period == null || scalar == 0) {
      return this
    }
    val instant = getChronology.add(period, getLocalMillis, scalar)
    withLocalMillis(instant)
  }

  def plus(period: ReadablePeriod): LocalTime = withPeriodAdded(period, 1)

  def plusHours(hours: Int): LocalTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().add(getLocalMillis, hours)
    withLocalMillis(instant)
  }

  def plusMinutes(minutes: Int): LocalTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().add(getLocalMillis, minutes)
    withLocalMillis(instant)
  }

  def plusSeconds(seconds: Int): LocalTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().add(getLocalMillis, seconds)
    withLocalMillis(instant)
  }

  def plusMillis(millis: Int): LocalTime = {
    if (millis == 0) {
      return this
    }
    val instant = getChronology.millis().add(getLocalMillis, millis)
    withLocalMillis(instant)
  }

  def minus(period: ReadablePeriod): LocalTime = withPeriodAdded(period, -1)

  def minusHours(hours: Int): LocalTime = {
    if (hours == 0) {
      return this
    }
    val instant = getChronology.hours().subtract(getLocalMillis, hours)
    withLocalMillis(instant)
  }

  def minusMinutes(minutes: Int): LocalTime = {
    if (minutes == 0) {
      return this
    }
    val instant = getChronology.minutes().subtract(getLocalMillis, minutes)
    withLocalMillis(instant)
  }

  def minusSeconds(seconds: Int): LocalTime = {
    if (seconds == 0) {
      return this
    }
    val instant = getChronology.seconds().subtract(getLocalMillis, seconds)
    withLocalMillis(instant)
  }

  def minusMillis(millis: Int): LocalTime = {
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

  def withHourOfDay(hour: Int): LocalTime = {
    withLocalMillis(getChronology.hourOfDay().set(getLocalMillis, hour))
  }

  def withMinuteOfHour(minute: Int): LocalTime = {
    withLocalMillis(getChronology.minuteOfHour().set(getLocalMillis, minute))
  }

  def withSecondOfMinute(second: Int): LocalTime = {
    withLocalMillis(getChronology.secondOfMinute().set(getLocalMillis, second))
  }

  def withMillisOfSecond(millis: Int): LocalTime = {
    withLocalMillis(getChronology.millisOfSecond().set(getLocalMillis, millis))
  }

  def withMillisOfDay(millis: Int): LocalTime = {
    withLocalMillis(getChronology.millisOfDay().set(getLocalMillis, millis))
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

  def toDateTimeToday(): DateTime = toDateTimeToday(null)

  def toDateTimeToday(zone: DateTimeZone): DateTime = {
    val chrono = getChronology.withZone(zone)
    val instantMillis = DateTimeUtils.currentTimeMillis()
    val resolved = chrono.set(this, instantMillis)
    new DateTime(resolved, chrono)
  }

  @ToString
  override def toString(): String = ISODateTimeFormat.time().print(this)

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
