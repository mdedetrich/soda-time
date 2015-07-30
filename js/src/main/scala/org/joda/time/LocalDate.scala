package org.joda.time

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Locale
import org.joda.convert.{ToString, FromString}
import org.joda.time.base.BaseLocal
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.field.AbstractReadableInstantFieldProperty
import org.joda.time.field.FieldUtils
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import LocalDate._
import scalajs.js

object LocalDate {

  private val YEAR = 0
  private val MONTH_OF_YEAR = 1
  private val DAY_OF_MONTH = 2
  private val DATE_DURATION_TYPES = new collection.mutable.HashSet[DurationFieldType]()

  DATE_DURATION_TYPES.add(DurationFieldType.days())
  DATE_DURATION_TYPES.add(DurationFieldType.weeks())
  DATE_DURATION_TYPES.add(DurationFieldType.months())
  DATE_DURATION_TYPES.add(DurationFieldType.weekyears())
  DATE_DURATION_TYPES.add(DurationFieldType.years())
  DATE_DURATION_TYPES.add(DurationFieldType.centuries())
  DATE_DURATION_TYPES.add(DurationFieldType.eras())

  def now(): LocalDate = new LocalDate()

  def now(zone: DateTimeZone): LocalDate = {
    if (zone == null) {
      throw new NullPointerException("Zone must not be null")
    }
    new LocalDate(zone)
  }

  def now(chronology: Chronology): LocalDate = {
    if (chronology == null) {
      throw new NullPointerException("Chronology must not be null")
    }
    new LocalDate(chronology)
  }

  @FromString
  def parse(str: String): LocalDate = {
    parse(str, ISODateTimeFormat.localDateParser())
  }

  def parse(str: String, formatter: DateTimeFormatter): LocalDate = formatter.parseLocalDate(str)
  
  def fromDateFields(date: js.Date): LocalDate = {
    if (date == null) {
      throw new IllegalArgumentException("The date must not be null")
    }
    new LocalDate(date.getFullYear(), date.getMonth +1, date.getDate())
  }

  @SerialVersionUID(-3193829732634L)
  class Property(@transient private var iInstant: LocalDate, @transient private var iField: DateTimeField)
    extends AbstractReadableInstantFieldProperty() {

    private def writeObject(oos: ObjectOutputStream) {
      oos.writeObject(iInstant)
      oos.writeObject(iField.getType)
    }

    private def readObject(oos: ObjectInputStream) {
      iInstant = oos.readObject().asInstanceOf[LocalDate]
      val `type` = oos.readObject().asInstanceOf[DateTimeFieldType]
      iField = `type`.getField(iInstant.getChronology)
    }

    def getField(): DateTimeField = iField

    protected def getMillis(): Long = iInstant.getLocalMillis

    protected override def getChronology(): Chronology = iInstant.getChronology

    def getLocalDate(): LocalDate = iInstant

    def addToCopy(value: Int): LocalDate = {
      iInstant.withLocalMillis(iField.add(iInstant.getLocalMillis, value))
    }

    def addWrapFieldToCopy(value: Int): LocalDate = {
      iInstant.withLocalMillis(iField.addWrapField(iInstant.getLocalMillis, value))
    }

    def setCopy(value: Int): LocalDate = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, value))
    }

    def setCopy(text: String, locale: Locale): LocalDate = {
      iInstant.withLocalMillis(iField.set(iInstant.getLocalMillis, text, locale))
    }

    def setCopy(text: String): LocalDate = setCopy(text, null)

    def withMaximumValue(): LocalDate = setCopy(getMaximumValue)

    def withMinimumValue(): LocalDate = setCopy(getMinimumValue)

    def roundFloorCopy(): LocalDate = {
      iInstant.withLocalMillis(iField.roundFloor(iInstant.getLocalMillis))
    }

    def roundCeilingCopy(): LocalDate = {
      iInstant.withLocalMillis(iField.roundCeiling(iInstant.getLocalMillis))
    }

    def roundHalfFloorCopy(): LocalDate = {
      iInstant.withLocalMillis(iField.roundHalfFloor(iInstant.getLocalMillis))
    }

    def roundHalfCeilingCopy(): LocalDate = {
      iInstant.withLocalMillis(iField.roundHalfCeiling(iInstant.getLocalMillis))
    }

    def roundHalfEvenCopy(): LocalDate = {
      iInstant.withLocalMillis(iField.roundHalfEven(iInstant.getLocalMillis))
    }
  }
}

@SerialVersionUID(-8775358157899L)
class LocalDate(instant: Long, private var chronology: Chronology) extends BaseLocal with ReadablePartial with Serializable {

  val localMillis = chronology.getZone.getMillisKeepLocal(DateTimeZone.UTC, instant)


  private var iChronology: Chronology = null
  private var iLocalMillis = chronology.dayOfMonth().roundFloor(localMillis)

  @transient private var iHash: Int = _

  chronology = DateTimeUtils.getChronology(chronology)

  iChronology = chronology

  chronology = chronology.withUTC()

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
    val values = converter.getPartialValues(this, instant, chronology, ISODateTimeFormat.localDateParser())
    iLocalMillis = iChronology.getDateTimeMillis(values(0), values(1), values(2), 0)
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    val values = converter.getPartialValues(this, instant, _chronology, ISODateTimeFormat.localDateParser())
    iLocalMillis = iChronology.getDateTimeMillis(values(0), values(1), values(2), 0)
  }

  def this(instant: AnyRef) {
    this(instant, null.asInstanceOf[Chronology])
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           chronology: Chronology) {
    this()
    var _chronology: Chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology).withUTC()
    val instant = _chronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, 0)
    iChronology = _chronology
    iLocalMillis = instant
  }

  def this(year: Int, monthOfYear: Int, dayOfMonth: Int) {
    this(year, monthOfYear, dayOfMonth, ISOChronology.getInstanceUTC)
  }

  private def readResolve(): AnyRef = {
    if (iChronology == null) {
      return new LocalDate(iLocalMillis, ISOChronology.getInstanceUTC)
    }
    if (DateTimeZone.UTC == iChronology.getZone == false) {
      return new LocalDate(iLocalMillis, iChronology.withUTC())
    }
    this
  }

  def size(): Int = 3

  protected def getField(index: Int, chrono: Chronology): DateTimeField = index match {
    case YEAR => chrono.year()
    case MONTH_OF_YEAR => chrono.monthOfYear()
    case DAY_OF_MONTH => chrono.dayOfMonth()
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  def getValue(index: Int): Int = index match {
    case YEAR => getChronology.year().get(getLocalMillis)
    case MONTH_OF_YEAR => getChronology.monthOfYear().get(getLocalMillis)
    case DAY_OF_MONTH => getChronology.dayOfMonth().get(getLocalMillis)
    case _ => throw new IndexOutOfBoundsException("Invalid index: " + index)
  }

  override def get(fieldType: DateTimeFieldType): Int = {
    if (fieldType == null) {
      throw new IllegalArgumentException("The DateTimeFieldType must not be null")
    }
    if (isSupported(fieldType) == false) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    fieldType.getField(getChronology).get(getLocalMillis)
  }

  override def isSupported(`type`: DateTimeFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    val durType = `type`.getDurationType
    if (DATE_DURATION_TYPES.contains(durType) ||
      durType.getField(getChronology).getUnitMillis >= getChronology.days().getUnitMillis) {
      return `type`.getField(getChronology).isSupported
    }
    false
  }

  def isSupported(`type`: DurationFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    val field = `type`.getField(getChronology)
    if (DATE_DURATION_TYPES.contains(`type`) ||
      field.getUnitMillis >= getChronology.days().getUnitMillis) {
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
    if (partial.isInstanceOf[LocalDate]) {
      val other = partial.asInstanceOf[LocalDate]
      if (iChronology == other.iChronology) {
        return iLocalMillis == other.iLocalMillis
      }
    }
    this == partial
  }

  override def hashCode(): Int = {
    var hash = iHash
    if (hash == 0) {
      iHash = super.hashCode
      hash = iHash
    }
    hash
  }

  override def compareTo(partial: ReadablePartial): Int = {
    if (this == partial) {
      return 0
    }
    if (partial.isInstanceOf[LocalDate]) {
      val other = partial.asInstanceOf[LocalDate]
      if (iChronology == other.iChronology) {
        return if (iLocalMillis < other.iLocalMillis) -1 else if (iLocalMillis == other.iLocalMillis) 0 else 1
      }
    }
    super.compareTo(partial)
  }

  def toDateTimeAtStartOfDay(): DateTime = toDateTimeAtStartOfDay(null)

  def toDateTimeAtStartOfDay(zone: DateTimeZone): DateTime = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val chrono = getChronology.withZone(_zone)
    val localMillis = getLocalMillis + 6L * DateTimeConstants.MILLIS_PER_HOUR
    var instant = _zone.convertLocalToUTC(localMillis, false)
    instant = chrono.dayOfMonth().roundFloor(instant)
    new DateTime(instant, chrono)
  }

  @Deprecated
  def toDateTimeAtMidnight(): DateTime = toDateTimeAtMidnight(null)

  @Deprecated
  def toDateTimeAtMidnight(zone: DateTimeZone): DateTime = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val chrono = getChronology.withZone(_zone)
    new DateTime(getYear, getMonthOfYear, getDayOfMonth, 0, 0, 0, 0, chrono)
  }

  def toDateTimeAtCurrentTime(): DateTime = toDateTimeAtCurrentTime(null)

  def toDateTimeAtCurrentTime(zone: DateTimeZone): DateTime = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val chrono = getChronology.withZone(_zone)
    val instantMillis = DateTimeUtils.currentTimeMillis()
    val resolved = chrono.set(this, instantMillis)
    new DateTime(resolved, chrono)
  }

  @Deprecated
  def toDateMidnight(): DateMidnight = toDateMidnight(null)

  @Deprecated
  def toDateMidnight(zone: DateTimeZone): DateMidnight = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val chrono = getChronology.withZone(_zone)
    new DateMidnight(getYear, getMonthOfYear, getDayOfMonth, chrono)
  }

  def toLocalDateTime(time: LocalTime): LocalDateTime = {
    if (time == null) {
      throw new IllegalArgumentException("The time must not be null")
    }
    if (getChronology != time.getChronology) {
      throw new IllegalArgumentException("The chronology of the time does not match")
    }
    val localMillis = getLocalMillis + time.getLocalMillis
    new LocalDateTime(localMillis, getChronology)
  }

  def toDateTime(time: LocalTime): DateTime = toDateTime(time, null)

  def toDateTime(time: LocalTime, zone: DateTimeZone): DateTime = {
    if (time == null) {
      return toDateTimeAtCurrentTime(zone)
    }
    if (getChronology != time.getChronology) {
      throw new IllegalArgumentException("The chronology of the time does not match")
    }
    val chrono = getChronology.withZone(zone)
    new DateTime(getYear, getMonthOfYear, getDayOfMonth, time.getHourOfDay, time.getMinuteOfHour, time.getSecondOfMinute,
      time.getMillisOfSecond, chrono)
  }

  def toInterval(): Interval = toInterval(null)

  def toInterval(zone: DateTimeZone): Interval = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    val start = toDateTimeAtStartOfDay(_zone)
    val end = plusDays(1).toDateTimeAtStartOfDay(_zone)
    new Interval(start, end)
  }

  def toDate(): js.Date = {
    val dom = getDayOfMonth
    var date = new js.Date(getYear, getMonthOfYear - 1, dom)
    var check = LocalDate.fromDateFields(date)
    if (check.isBefore(this)) {
      while (check == this == false) {
        date.setTime(date.getTime + 3600000)
        check = LocalDate.fromDateFields(date)
      }
      while (date.getDate == dom) {
        date.setTime(date.getTime - 1000)
      }
      date.setTime(date.getTime + 1000)
    } else if (check == this) {
      val earlier = new js.Date(date.getTime)
      if (earlier.getDate == dom) {
        date = earlier
      }
    }
    date
  }

  def withLocalMillis(newMillis: Long): LocalDate = {
    var _newMillis: Long = newMillis
    _newMillis = iChronology.dayOfMonth().roundFloor(_newMillis)
    if (_newMillis == getLocalMillis) this else new LocalDate(_newMillis, getChronology)
  }

  def withFields(partial: ReadablePartial): LocalDate = {
    if (partial == null) {
      return this
    }
    withLocalMillis(getChronology.set(partial, getLocalMillis))
  }

  def withField(fieldType: DateTimeFieldType, value: Int): LocalDate = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (isSupported(fieldType) == false) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    val instant = fieldType.getField(getChronology).set(getLocalMillis, value)
    withLocalMillis(instant)
  }

  def withFieldAdded(fieldType: DurationFieldType, amount: Int): LocalDate = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field must not be null")
    }
    if (isSupported(fieldType) == false) {
      throw new IllegalArgumentException("Field '" + fieldType + "' is not supported")
    }
    if (amount == 0) {
      return this
    }
    val instant = fieldType.getField(getChronology).add(getLocalMillis, amount)
    withLocalMillis(instant)
  }

  def withPeriodAdded(period: ReadablePeriod, scalar: Int): LocalDate = {
    if (period == null || scalar == 0) {
      return this
    }
    var instant = getLocalMillis
    val chrono = getChronology
    for (i <- 0 until period.size) {
      val value = FieldUtils.safeMultiply(period.getValue(i), scalar)
      val `type` = period.getFieldType(i)
      if (isSupported(`type`)) {
        instant = `type`.getField(chrono).add(instant, value)
      }
    }
    withLocalMillis(instant)
  }

  def plus(period: ReadablePeriod): LocalDate = withPeriodAdded(period, 1)

  def plusYears(years: Int): LocalDate = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().add(getLocalMillis, years)
    withLocalMillis(instant)
  }

  def plusMonths(months: Int): LocalDate = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().add(getLocalMillis, months)
    withLocalMillis(instant)
  }

  def plusWeeks(weeks: Int): LocalDate = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().add(getLocalMillis, weeks)
    withLocalMillis(instant)
  }

  def plusDays(days: Int): LocalDate = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().add(getLocalMillis, days)
    withLocalMillis(instant)
  }

  def minus(period: ReadablePeriod): LocalDate = withPeriodAdded(period, -1)

  def minusYears(years: Int): LocalDate = {
    if (years == 0) {
      return this
    }
    val instant = getChronology.years().subtract(getLocalMillis, years)
    withLocalMillis(instant)
  }

  def minusMonths(months: Int): LocalDate = {
    if (months == 0) {
      return this
    }
    val instant = getChronology.months().subtract(getLocalMillis, months)
    withLocalMillis(instant)
  }

  def minusWeeks(weeks: Int): LocalDate = {
    if (weeks == 0) {
      return this
    }
    val instant = getChronology.weeks().subtract(getLocalMillis, weeks)
    withLocalMillis(instant)
  }

  def minusDays(days: Int): LocalDate = {
    if (days == 0) {
      return this
    }
    val instant = getChronology.days().subtract(getLocalMillis, days)
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

  def withEra(era: Int): LocalDate = {
    withLocalMillis(getChronology.era().set(getLocalMillis, era))
  }

  def withCenturyOfEra(centuryOfEra: Int): LocalDate = {
    withLocalMillis(getChronology.centuryOfEra().set(getLocalMillis, centuryOfEra))
  }

  def withYearOfEra(yearOfEra: Int): LocalDate = {
    withLocalMillis(getChronology.yearOfEra().set(getLocalMillis, yearOfEra))
  }

  def withYearOfCentury(yearOfCentury: Int): LocalDate = {
    withLocalMillis(getChronology.yearOfCentury().set(getLocalMillis, yearOfCentury))
  }

  def withYear(year: Int): LocalDate = {
    withLocalMillis(getChronology.year().set(getLocalMillis, year))
  }

  def withWeekyear(weekyear: Int): LocalDate = {
    withLocalMillis(getChronology.weekyear().set(getLocalMillis, weekyear))
  }

  def withMonthOfYear(monthOfYear: Int): LocalDate = {
    withLocalMillis(getChronology.monthOfYear().set(getLocalMillis, monthOfYear))
  }

  def withWeekOfWeekyear(weekOfWeekyear: Int): LocalDate = {
    withLocalMillis(getChronology.weekOfWeekyear().set(getLocalMillis, weekOfWeekyear))
  }

  def withDayOfYear(dayOfYear: Int): LocalDate = {
    withLocalMillis(getChronology.dayOfYear().set(getLocalMillis, dayOfYear))
  }

  def withDayOfMonth(dayOfMonth: Int): LocalDate = {
    withLocalMillis(getChronology.dayOfMonth().set(getLocalMillis, dayOfMonth))
  }

  def withDayOfWeek(dayOfWeek: Int): LocalDate = {
    withLocalMillis(getChronology.dayOfWeek().set(getLocalMillis, dayOfWeek))
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

  @ToString
  override def toString(): String = ISODateTimeFormat.date().print(this)

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
