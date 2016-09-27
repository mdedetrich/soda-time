package org.joda.time

import java.io.Serializable
import java.util.Calendar
import java.util.Date
import java.util.Locale
import org.joda.time.base.BasePartial
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.AbstractPartialFieldProperty
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISODateTimeFormat
import TimeOfDay._

object TimeOfDay {

  private val FIELD_TYPES = Array(DateTimeFieldType.hourOfDay(),
                                  DateTimeFieldType.minuteOfHour(),
                                  DateTimeFieldType.secondOfMinute(),
                                  DateTimeFieldType.millisOfSecond())

  val MIDNIGHT = new TimeOfDay(0, 0, 0, 0)
  val HOUR_OF_DAY = 0
  val MINUTE_OF_HOUR = 1
  val SECOND_OF_MINUTE = 2
  val MILLIS_OF_SECOND = 3

  def fromCalendarFields(calendar: Calendar): TimeOfDay = {
    if (calendar == null) {
      throw new IllegalArgumentException("The calendar must not be null")
    }
    new TimeOfDay(calendar.get(Calendar.HOUR_OF_DAY),
                  calendar.get(Calendar.MINUTE),
                  calendar.get(Calendar.SECOND),
                  calendar.get(Calendar.MILLISECOND))
  }

  def fromDateFields(date: Date): TimeOfDay = {
    if (date == null) {
      throw new IllegalArgumentException("The date must not be null")
    }
    new TimeOfDay(date.getHours,
                  date.getMinutes,
                  date.getSeconds,
                  ((date.getTime % 1000).toInt + 1000) % 1000)
  }

  def fromMillisOfDay(millisOfDay: Long): TimeOfDay =
    fromMillisOfDay(millisOfDay, null)

  def fromMillisOfDay(millisOfDay: Long, chrono: Chronology): TimeOfDay = {
    var _chrono = chrono
    _chrono = DateTimeUtils.getChronology(_chrono)
    _chrono = _chrono.withUTC()
    new TimeOfDay(millisOfDay, _chrono)
  }

  @SerialVersionUID(5598459141741063833L)
  @Deprecated
  class Property(private val iTimeOfDay: TimeOfDay,
                 private val iFieldIndex: Int)
      extends AbstractPartialFieldProperty()
      with Serializable {

    def getField(): DateTimeField = iTimeOfDay.getField(iFieldIndex)

    protected def getReadablePartial(): ReadablePartial = iTimeOfDay

    def getTimeOfDay(): TimeOfDay = iTimeOfDay

    def get(): Int = iTimeOfDay.getValue(iFieldIndex)

    def addToCopy(valueToAdd: Int): TimeOfDay = {
      var newValues = iTimeOfDay.getValues
      newValues =
        getField.addWrapPartial(iTimeOfDay, iFieldIndex, newValues, valueToAdd)
      new TimeOfDay(iTimeOfDay, newValues)
    }

    def addNoWrapToCopy(valueToAdd: Int): TimeOfDay = {
      var newValues = iTimeOfDay.getValues
      newValues = getField.add(iTimeOfDay, iFieldIndex, newValues, valueToAdd)
      new TimeOfDay(iTimeOfDay, newValues)
    }

    def addWrapFieldToCopy(valueToAdd: Int): TimeOfDay = {
      var newValues = iTimeOfDay.getValues
      newValues =
        getField.addWrapField(iTimeOfDay, iFieldIndex, newValues, valueToAdd)
      new TimeOfDay(iTimeOfDay, newValues)
    }

    def setCopy(value: Int): TimeOfDay = {
      var newValues = iTimeOfDay.getValues
      newValues = getField.set(iTimeOfDay, iFieldIndex, newValues, value)
      new TimeOfDay(iTimeOfDay, newValues)
    }

    def setCopy(text: String, locale: Locale): TimeOfDay = {
      var newValues = iTimeOfDay.getValues
      newValues =
        getField.set(iTimeOfDay, iFieldIndex, newValues, text, locale)
      new TimeOfDay(iTimeOfDay, newValues)
    }

    def setCopy(text: String): TimeOfDay = setCopy(text, null)

    def withMaximumValue(): TimeOfDay = setCopy(getMaximumValue)

    def withMinimumValue(): TimeOfDay = setCopy(getMinimumValue)
  }
}

@SerialVersionUID(3633353405803318660L)
@Deprecated
class TimeOfDay extends BasePartial() with ReadablePartial with Serializable {

  def this(zone: DateTimeZone) {
    this()
    super.auxConstructor(ISOChronology.getInstance(zone))
  }

  def this(chronology: Chronology) {
    this()
    super.auxConstructor(chronology)
  }

  def this(instant: Long) {
    this()
    super.auxConstructor(instant)
  }

  def this(instant: Long, chronology: Chronology) {
    this()
    super.auxConstructor(instant, chronology)
  }

  def this(instant: AnyRef) {
    this()
    super.auxConstructor(instant, null, ISODateTimeFormat.timeParser())
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    super.auxConstructor(instant,
                         DateTimeUtils.getChronology(chronology),
                         ISODateTimeFormat.timeParser())
  }

  def this(hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           chronology: Chronology) {
    this()
    super.auxConstructor(
      Array(hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond),
      chronology)
  }

  def this(partial: TimeOfDay, values: Array[Int]) {
    this()
    super.auxConstructor(partial, values)
  }

  def this(partial: TimeOfDay, chrono: Chronology) {
    this()
    super.auxConstructor(partial, chrono)
  }

  def this(hourOfDay: Int, minuteOfHour: Int) {
    this(hourOfDay, minuteOfHour, 0, 0, null)
  }

  def this(hourOfDay: Int, minuteOfHour: Int, chronology: Chronology) {
    this(hourOfDay, minuteOfHour, 0, 0, chronology)
  }

  def this(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int) {
    this(hourOfDay, minuteOfHour, secondOfMinute, 0, null)
  }

  def this(hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           chronology: Chronology) {
    this(hourOfDay, minuteOfHour, secondOfMinute, 0, chronology)
  }

  def this(hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this(hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, null)
  }

  def size(): Int = 4

  protected def getField(index: Int, chrono: Chronology): DateTimeField =
    index match {
      case HOUR_OF_DAY => chrono.hourOfDay()
      case MINUTE_OF_HOUR => chrono.minuteOfHour()
      case SECOND_OF_MINUTE => chrono.secondOfMinute()
      case MILLIS_OF_SECOND => chrono.millisOfSecond()
      case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
    }

  override def getFieldType(index: Int): DateTimeFieldType = FIELD_TYPES(index)

  override def getFieldTypes(): Array[DateTimeFieldType] = {
    FIELD_TYPES.clone().asInstanceOf[Array[DateTimeFieldType]]
  }

  def withChronologyRetainFields(newChronology: Chronology): TimeOfDay = {
    var _newChronology = newChronology
    _newChronology = DateTimeUtils.getChronology(_newChronology)
    _newChronology = _newChronology.withUTC()
    if (_newChronology == getChronology) {
      this
    } else {
      val newTimeOfDay = new TimeOfDay(this, _newChronology)
      _newChronology.validate(newTimeOfDay, getValues)
      newTimeOfDay
    }
  }

  def withField(fieldType: DateTimeFieldType, value: Int): TimeOfDay = {
    val index = indexOfSupported(fieldType)
    if (value == getValue(index)) {
      return this
    }
    var newValues = getValues
    newValues = getField(index).set(this, index, newValues, value)
    new TimeOfDay(this, newValues)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): TimeOfDay = {
    val index = indexOfSupported(fieldType)
    if (amount == 0) {
      return this
    }
    var newValues = getValues
    newValues = getField(index).addWrapPartial(this, index, newValues, amount)
    new TimeOfDay(this, newValues)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): TimeOfDay = {
    if (period == null || scalar == 0) {
      return this
    }
    var newValues = getValues
    for (i <- 0 until period.size) {
      val fieldType = period.getFieldType(i)
      val index = indexOf(fieldType)
      if (index >= 0) {
        newValues = getField(index).addWrapPartial(
          this,
          index,
          newValues,
          FieldUtils.safeMultiply(period.getValue(i), scalar))
      }
    }
    new TimeOfDay(this, newValues)
  }

  def plus(period: ReadablePeriod): TimeOfDay = withPeriodAdded(period, 1)

  def plusHours(hours: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.hours(), hours)
  }

  def plusMinutes(minutes: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.minutes(), minutes)
  }

  def plusSeconds(seconds: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.seconds(), seconds)
  }

  def plusMillis(millis: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.millis(), millis)
  }

  def minus(period: ReadablePeriod): TimeOfDay = withPeriodAdded(period, -1)

  def minusHours(hours: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.hours(), FieldUtils.safeNegate(hours))
  }

  def minusMinutes(minutes: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.minutes(), FieldUtils.safeNegate(minutes))
  }

  def minusSeconds(seconds: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.seconds(), FieldUtils.safeNegate(seconds))
  }

  def minusMillis(millis: Int): TimeOfDay = {
    withFieldAdded(DurationFieldType.millis(), FieldUtils.safeNegate(millis))
  }

  def property(`type`: DateTimeFieldType): Property = {
    new Property(this, indexOfSupported(`type`))
  }

  def toLocalTime(): LocalTime = {
    new LocalTime(getHourOfDay,
                  getMinuteOfHour,
                  getSecondOfMinute,
                  getMillisOfSecond,
                  getChronology)
  }

  def toDateTimeToday(): DateTime = toDateTimeToday(null)

  def toDateTimeToday(zone: DateTimeZone): DateTime = {
    val chrono = getChronology.withZone(zone)
    val instantMillis = DateTimeUtils.currentTimeMillis()
    val resolved = chrono.set(this, instantMillis)
    new DateTime(resolved, chrono)
  }

  def getHourOfDay(): Int = getValue(HOUR_OF_DAY)

  def getMinuteOfHour(): Int = getValue(MINUTE_OF_HOUR)

  def getSecondOfMinute(): Int = getValue(SECOND_OF_MINUTE)

  def getMillisOfSecond(): Int = getValue(MILLIS_OF_SECOND)

  def withHourOfDay(hour: Int): TimeOfDay = {
    var newValues = getValues
    newValues =
      getChronology.hourOfDay().set(this, HOUR_OF_DAY, newValues, hour)
    new TimeOfDay(this, newValues)
  }

  def withMinuteOfHour(minute: Int): TimeOfDay = {
    var newValues = getValues
    newValues =
      getChronology.minuteOfHour().set(this, MINUTE_OF_HOUR, newValues, minute)
    new TimeOfDay(this, newValues)
  }

  def withSecondOfMinute(second: Int): TimeOfDay = {
    var newValues = getValues
    newValues = getChronology
      .secondOfMinute()
      .set(this, SECOND_OF_MINUTE, newValues, second)
    new TimeOfDay(this, newValues)
  }

  def withMillisOfSecond(millis: Int): TimeOfDay = {
    var newValues = getValues
    newValues = getChronology
      .millisOfSecond()
      .set(this, MILLIS_OF_SECOND, newValues, millis)
    new TimeOfDay(this, newValues)
  }

  def hourOfDay(): Property = new Property(this, HOUR_OF_DAY)

  def minuteOfHour(): Property = new Property(this, MINUTE_OF_HOUR)

  def secondOfMinute(): Property = new Property(this, SECOND_OF_MINUTE)

  def millisOfSecond(): Property = new Property(this, MILLIS_OF_SECOND)

  override def toString(): String = ISODateTimeFormat.tTime().print(this)
}
