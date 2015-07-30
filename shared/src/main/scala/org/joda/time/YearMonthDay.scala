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
import YearMonthDay._

object YearMonthDay {

  private val FIELD_TYPES = Array(DateTimeFieldType.year(), DateTimeFieldType.monthOfYear(), DateTimeFieldType.dayOfMonth())

  val YEAR = 0
  val MONTH_OF_YEAR = 1
  val DAY_OF_MONTH = 2

  def fromCalendarFields(calendar: Calendar): YearMonthDay = {
    if (calendar == null) {
      throw new IllegalArgumentException("The calendar must not be null")
    }
    new YearMonthDay(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1, calendar.get(Calendar.DAY_OF_MONTH))
  }

  def fromDateFields(date: Date): YearMonthDay = {
    if (date == null) {
      throw new IllegalArgumentException("The date must not be null")
    }
    new YearMonthDay(date.getYear + 1900, date.getMonth + 1, date.getDate)
  }

  @SerialVersionUID(5727734012190224363L)
  @Deprecated
  class Property(private val iYearMonthDay: YearMonthDay, private val iFieldIndex: Int)
    extends AbstractPartialFieldProperty() with Serializable {

    def getField(): DateTimeField = iYearMonthDay.getField(iFieldIndex)

    protected def getReadablePartial(): ReadablePartial = iYearMonthDay

    def getYearMonthDay(): YearMonthDay = iYearMonthDay

    def get(): Int = iYearMonthDay.getValue(iFieldIndex)

    def addToCopy(valueToAdd: Int): YearMonthDay = {
      var newValues = iYearMonthDay.getValues
      newValues = getField.add(iYearMonthDay, iFieldIndex, newValues, valueToAdd)
      new YearMonthDay(iYearMonthDay, newValues)
    }

    def addWrapFieldToCopy(valueToAdd: Int): YearMonthDay = {
      var newValues = iYearMonthDay.getValues
      newValues = getField.addWrapField(iYearMonthDay, iFieldIndex, newValues, valueToAdd)
      new YearMonthDay(iYearMonthDay, newValues)
    }

    def setCopy(value: Int): YearMonthDay = {
      var newValues = iYearMonthDay.getValues
      newValues = getField.set(iYearMonthDay, iFieldIndex, newValues, value)
      new YearMonthDay(iYearMonthDay, newValues)
    }

    def setCopy(text: String, locale: Locale): YearMonthDay = {
      var newValues = iYearMonthDay.getValues
      newValues = getField.set(iYearMonthDay, iFieldIndex, newValues, text, locale)
      new YearMonthDay(iYearMonthDay, newValues)
    }

    def setCopy(text: String): YearMonthDay = setCopy(text, null)

    def withMaximumValue(): YearMonthDay = setCopy(getMaximumValue)

    def withMinimumValue(): YearMonthDay = setCopy(getMinimumValue)
  }
}

@SerialVersionUID(797544782896179L)
@Deprecated
class YearMonthDay extends BasePartial() with ReadablePartial with Serializable {

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
    super.auxConstructor(instant, null, ISODateTimeFormat.dateOptionalTimeParser())
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    super.auxConstructor(instant, DateTimeUtils.getChronology(chronology), ISODateTimeFormat.dateOptionalTimeParser())
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           chronology: Chronology) {
    this()
    super.auxConstructor(Array(year, monthOfYear, dayOfMonth), chronology)
  }

  def this(year: Int, monthOfYear: Int, dayOfMonth: Int) {
    this(year, monthOfYear, dayOfMonth, null)
  }

  def this(partial: YearMonthDay, values: Array[Int]) {
    this()
    super.auxConstructor(partial, values)
  }

  def this(partial: YearMonthDay, chrono: Chronology) {
    this()
    super.auxConstructor(partial, chrono)
  }

  def size(): Int = 3

  protected def getField(index: Int, chrono: Chronology): DateTimeField = index match {
    case YEAR => chrono.year()
    case MONTH_OF_YEAR => chrono.monthOfYear()
    case DAY_OF_MONTH => chrono.dayOfMonth()
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  override def getFieldType(index: Int): DateTimeFieldType = FIELD_TYPES(index)

  override def getFieldTypes(): Array[DateTimeFieldType] = {
    FIELD_TYPES.clone().asInstanceOf[Array[DateTimeFieldType]]
  }

  def withChronologyRetainFields(newChronology: Chronology): YearMonthDay = {
    var _newChronology = newChronology
    _newChronology = DateTimeUtils.getChronology(_newChronology)
    _newChronology = _newChronology.withUTC()
    if (_newChronology == getChronology) {
      this
    } else {
      val newYearMonthDay = new YearMonthDay(this, _newChronology)
      _newChronology.validate(newYearMonthDay, getValues)
      newYearMonthDay
    }
  }

  def withField(fieldType: DateTimeFieldType, value: Int): YearMonthDay = {
    val index = indexOfSupported(fieldType)
    if (value == getValue(index)) {
      return this
    }
    var newValues = getValues
    newValues = getField(index).set(this, index, newValues, value)
    new YearMonthDay(this, newValues)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): YearMonthDay = {
    val index = indexOfSupported(fieldType)
    if (amount == 0) {
      return this
    }
    var newValues = getValues
    newValues = getField(index).add(this, index, newValues, amount)
    new YearMonthDay(this, newValues)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): YearMonthDay = {
    if (period == null || scalar == 0) {
      return this
    }
    var newValues = getValues
    for (i <- 0 until period.size) {
      val fieldType = period.getFieldType(i)
      val index = indexOf(fieldType)
      if (index >= 0) {
        newValues = getField(index).add(this, index, newValues, FieldUtils.safeMultiply(period.getValue(i),
          scalar))
      }
    }
    new YearMonthDay(this, newValues)
  }

  def plus(period: ReadablePeriod): YearMonthDay = withPeriodAdded(period, 1)

  def plusYears(years: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.years(), years)
  }

  def plusMonths(months: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.months(), months)
  }

  def plusDays(days: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.days(), days)
  }

  def minus(period: ReadablePeriod): YearMonthDay = withPeriodAdded(period, -1)

  def minusYears(years: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.years(), FieldUtils.safeNegate(years))
  }

  def minusMonths(months: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.months(), FieldUtils.safeNegate(months))
  }

  def minusDays(days: Int): YearMonthDay = {
    withFieldAdded(DurationFieldType.days(), FieldUtils.safeNegate(days))
  }

  def property(`type`: DateTimeFieldType): Property = {
    new Property(this, indexOfSupported(`type`))
  }

  def toLocalDate(): LocalDate = {
    new LocalDate(getYear, getMonthOfYear, getDayOfMonth, getChronology)
  }

  def toDateTimeAtMidnight(): DateTime = toDateTimeAtMidnight(null)

  def toDateTimeAtMidnight(zone: DateTimeZone): DateTime = {
    val chrono = getChronology.withZone(zone)
    new DateTime(getYear, getMonthOfYear, getDayOfMonth, 0, 0, 0, 0, chrono)
  }

  def toDateTimeAtCurrentTime(): DateTime = toDateTimeAtCurrentTime(null)

  def toDateTimeAtCurrentTime(zone: DateTimeZone): DateTime = {
    val chrono = getChronology.withZone(zone)
    val instantMillis = DateTimeUtils.currentTimeMillis()
    val resolved = chrono.set(this, instantMillis)
    new DateTime(resolved, chrono)
  }

  def toDateMidnight(): DateMidnight = toDateMidnight(null)

  def toDateMidnight(zone: DateTimeZone): DateMidnight = {
    val chrono = getChronology.withZone(zone)
    new DateMidnight(getYear, getMonthOfYear, getDayOfMonth, chrono)
  }

  def toDateTime(time: TimeOfDay): DateTime = toDateTime(time, null)

  def toDateTime(time: TimeOfDay, zone: DateTimeZone): DateTime = {
    val chrono = getChronology.withZone(zone)
    var instant = DateTimeUtils.currentTimeMillis()
    instant = chrono.set(this, instant)
    if (time != null) {
      instant = chrono.set(time, instant)
    }
    new DateTime(instant, chrono)
  }

  def toInterval(): Interval = toInterval(null)

  def toInterval(zone: DateTimeZone): Interval = {
    var _zone = zone
    _zone = DateTimeUtils.getZone(_zone)
    toDateMidnight(_zone).toInterval()
  }

  def getYear(): Int = getValue(YEAR)

  def getMonthOfYear(): Int = getValue(MONTH_OF_YEAR)

  def getDayOfMonth(): Int = getValue(DAY_OF_MONTH)

  def withYear(year: Int): YearMonthDay = {
    var newValues = getValues
    newValues = getChronology.year().set(this, YEAR, newValues, year)
    new YearMonthDay(this, newValues)
  }

  def withMonthOfYear(monthOfYear: Int): YearMonthDay = {
    var newValues = getValues
    newValues = getChronology.monthOfYear().set(this, MONTH_OF_YEAR, newValues, monthOfYear)
    new YearMonthDay(this, newValues)
  }

  def withDayOfMonth(dayOfMonth: Int): YearMonthDay = {
    var newValues = getValues
    newValues = getChronology.dayOfMonth().set(this, DAY_OF_MONTH, newValues, dayOfMonth)
    new YearMonthDay(this, newValues)
  }

  def year(): Property = new Property(this, YEAR)

  def monthOfYear(): Property = new Property(this, MONTH_OF_YEAR)

  def dayOfMonth(): Property = new Property(this, DAY_OF_MONTH)

  override def toString(): String = {
    ISODateTimeFormat.yearMonthDay().print(this)
  }
}
