package org.joda.time

import java.io.Serializable

object DateTimeFieldType {

  val ERA:Byte = 1
  val YEAR_OF_ERA:Byte = 2
  val CENTURY_OF_ERA:Byte = 3
  val YEAR_OF_CENTURY:Byte = 4
  val YEAR:Byte = 5
  val DAY_OF_YEAR:Byte = 6
  val MONTH_OF_YEAR:Byte = 7
  val DAY_OF_MONTH:Byte = 8
  val WEEKYEAR_OF_CENTURY:Byte = 9
  val WEEKYEAR:Byte = 10
  val WEEK_OF_WEEKYEAR:Byte = 11
  val DAY_OF_WEEK:Byte = 12
  val HALFDAY_OF_DAY:Byte = 13
  val HOUR_OF_HALFDAY:Byte = 14
  val CLOCKHOUR_OF_HALFDAY:Byte = 15
  val CLOCKHOUR_OF_DAY:Byte = 16
  val HOUR_OF_DAY:Byte = 17
  val MINUTE_OF_DAY:Byte = 18
  val MINUTE_OF_HOUR:Byte = 19
  val SECOND_OF_DAY:Byte = 20
  val SECOND_OF_MINUTE:Byte = 21
  val MILLIS_OF_DAY:Byte = 22
  val MILLIS_OF_SECOND:Byte = 23

  private val ERA_TYPE = new StandardDateTimeFieldType("era", ERA, DurationFieldType.eras(), null)

  private val YEAR_OF_ERA_TYPE = new StandardDateTimeFieldType("yearOfEra", YEAR_OF_ERA, DurationFieldType.years(),
    DurationFieldType.eras())

  private val CENTURY_OF_ERA_TYPE = new StandardDateTimeFieldType("centuryOfEra", CENTURY_OF_ERA, DurationFieldType.centuries(),
    DurationFieldType.eras())

  private val YEAR_OF_CENTURY_TYPE = new StandardDateTimeFieldType("yearOfCentury", YEAR_OF_CENTURY,
    DurationFieldType.years(), DurationFieldType.centuries())

  private val YEAR_TYPE = new StandardDateTimeFieldType("year", YEAR, DurationFieldType.years(), null)

  private val DAY_OF_YEAR_TYPE = new StandardDateTimeFieldType("dayOfYear", DAY_OF_YEAR, DurationFieldType.days(),
    DurationFieldType.years())

  private val MONTH_OF_YEAR_TYPE = new StandardDateTimeFieldType("monthOfYear", MONTH_OF_YEAR, DurationFieldType.months(),
    DurationFieldType.years())

  private val DAY_OF_MONTH_TYPE = new StandardDateTimeFieldType("dayOfMonth", DAY_OF_MONTH, DurationFieldType.days(),
    DurationFieldType.months())

  private val WEEKYEAR_OF_CENTURY_TYPE = new StandardDateTimeFieldType("weekyearOfCentury", WEEKYEAR_OF_CENTURY,
    DurationFieldType.weekyears(), DurationFieldType.centuries())

  private val WEEKYEAR_TYPE = new StandardDateTimeFieldType("weekyear", WEEKYEAR, DurationFieldType.weekyears(),
    null)

  private val WEEK_OF_WEEKYEAR_TYPE = new StandardDateTimeFieldType("weekOfWeekyear", WEEK_OF_WEEKYEAR,
    DurationFieldType.weeks(), DurationFieldType.weekyears())

  private val DAY_OF_WEEK_TYPE = new StandardDateTimeFieldType("dayOfWeek", DAY_OF_WEEK, DurationFieldType.days(),
    DurationFieldType.weeks())

  private val HALFDAY_OF_DAY_TYPE = new StandardDateTimeFieldType("halfdayOfDay", HALFDAY_OF_DAY, DurationFieldType.halfdays(),
    DurationFieldType.days())

  private val HOUR_OF_HALFDAY_TYPE = new StandardDateTimeFieldType("hourOfHalfday", HOUR_OF_HALFDAY,
    DurationFieldType.hours(), DurationFieldType.halfdays())

  private val CLOCKHOUR_OF_HALFDAY_TYPE = new StandardDateTimeFieldType("clockhourOfHalfday", CLOCKHOUR_OF_HALFDAY,
    DurationFieldType.hours(), DurationFieldType.halfdays())

  private val CLOCKHOUR_OF_DAY_TYPE = new StandardDateTimeFieldType("clockhourOfDay", CLOCKHOUR_OF_DAY,
    DurationFieldType.hours(), DurationFieldType.days())

  private val HOUR_OF_DAY_TYPE = new StandardDateTimeFieldType("hourOfDay", HOUR_OF_DAY, DurationFieldType.hours(),
    DurationFieldType.days())

  private val MINUTE_OF_DAY_TYPE = new StandardDateTimeFieldType("minuteOfDay", MINUTE_OF_DAY, DurationFieldType.minutes(),
    DurationFieldType.days())

  private val MINUTE_OF_HOUR_TYPE = new StandardDateTimeFieldType("minuteOfHour", MINUTE_OF_HOUR, DurationFieldType.minutes(),
    DurationFieldType.hours())

  private val SECOND_OF_DAY_TYPE = new StandardDateTimeFieldType("secondOfDay", SECOND_OF_DAY, DurationFieldType.seconds(),
    DurationFieldType.days())

  private val SECOND_OF_MINUTE_TYPE = new StandardDateTimeFieldType("secondOfMinute", SECOND_OF_MINUTE,
    DurationFieldType.seconds(), DurationFieldType.minutes())

  private val MILLIS_OF_DAY_TYPE = new StandardDateTimeFieldType("millisOfDay", MILLIS_OF_DAY, DurationFieldType.millis(),
    DurationFieldType.days())

  private val MILLIS_OF_SECOND_TYPE = new StandardDateTimeFieldType("millisOfSecond", MILLIS_OF_SECOND,
    DurationFieldType.millis(), DurationFieldType.seconds())

  def millisOfSecond(): DateTimeFieldType = MILLIS_OF_SECOND_TYPE

  def millisOfDay(): DateTimeFieldType = MILLIS_OF_DAY_TYPE

  def secondOfMinute(): DateTimeFieldType = SECOND_OF_MINUTE_TYPE

  def secondOfDay(): DateTimeFieldType = SECOND_OF_DAY_TYPE

  def minuteOfHour(): DateTimeFieldType = MINUTE_OF_HOUR_TYPE

  def minuteOfDay(): DateTimeFieldType = MINUTE_OF_DAY_TYPE

  def hourOfDay(): DateTimeFieldType = HOUR_OF_DAY_TYPE

  def clockhourOfDay(): DateTimeFieldType = CLOCKHOUR_OF_DAY_TYPE

  def hourOfHalfday(): DateTimeFieldType = HOUR_OF_HALFDAY_TYPE

  def clockhourOfHalfday(): DateTimeFieldType = CLOCKHOUR_OF_HALFDAY_TYPE

  def halfdayOfDay(): DateTimeFieldType = HALFDAY_OF_DAY_TYPE

  def dayOfWeek(): DateTimeFieldType = DAY_OF_WEEK_TYPE

  def dayOfMonth(): DateTimeFieldType = DAY_OF_MONTH_TYPE

  def dayOfYear(): DateTimeFieldType = DAY_OF_YEAR_TYPE

  def weekOfWeekyear(): DateTimeFieldType = WEEK_OF_WEEKYEAR_TYPE

  def weekyear(): DateTimeFieldType = WEEKYEAR_TYPE

  def weekyearOfCentury(): DateTimeFieldType = WEEKYEAR_OF_CENTURY_TYPE

  def monthOfYear(): DateTimeFieldType = MONTH_OF_YEAR_TYPE

  def year(): DateTimeFieldType = YEAR_TYPE

  def yearOfEra(): DateTimeFieldType = YEAR_OF_ERA_TYPE

  def yearOfCentury(): DateTimeFieldType = YEAR_OF_CENTURY_TYPE

  def centuryOfEra(): DateTimeFieldType = CENTURY_OF_ERA_TYPE

  def era(): DateTimeFieldType = ERA_TYPE

  @SerialVersionUID(-9937958251642L)
  private class StandardDateTimeFieldType(name: String,
                                          private val iOrdinal: Byte,
                                          @transient private val iUnitType: DurationFieldType,
                                          @transient private val iRangeType: DurationFieldType) extends DateTimeFieldType(name) {

    def getDurationType(): DurationFieldType = iUnitType

    def getRangeDurationType(): DurationFieldType = iRangeType

    override def equals(obj: Any): Boolean = {
      if (this == obj) {
        return true
      }
      if (obj.isInstanceOf[StandardDateTimeFieldType]) {
        return iOrdinal ==
          obj.asInstanceOf[StandardDateTimeFieldType].iOrdinal
      }
      false
    }

    override def hashCode(): Int = 1 << iOrdinal

    def getField(chronology: Chronology): DateTimeField = {
      var _chronology: Chronology = chronology
      _chronology = DateTimeUtils.getChronology(_chronology)
      iOrdinal match {
        case ERA => _chronology.era()
        case YEAR_OF_ERA => _chronology.yearOfEra()
        case CENTURY_OF_ERA => _chronology.centuryOfEra()
        case YEAR_OF_CENTURY => _chronology.yearOfCentury()
        case YEAR => _chronology.year()
        case DAY_OF_YEAR => _chronology.dayOfYear()
        case MONTH_OF_YEAR => _chronology.monthOfYear()
        case DAY_OF_MONTH => _chronology.dayOfMonth()
        case WEEKYEAR_OF_CENTURY => _chronology.weekyearOfCentury()
        case WEEKYEAR => _chronology.weekyear()
        case WEEK_OF_WEEKYEAR => _chronology.weekOfWeekyear()
        case DAY_OF_WEEK => _chronology.dayOfWeek()
        case HALFDAY_OF_DAY => _chronology.halfdayOfDay()
        case HOUR_OF_HALFDAY => _chronology.hourOfHalfday()
        case CLOCKHOUR_OF_HALFDAY => _chronology.clockhourOfHalfday()
        case CLOCKHOUR_OF_DAY => _chronology.clockhourOfDay()
        case HOUR_OF_DAY => _chronology.hourOfDay()
        case MINUTE_OF_DAY => _chronology.minuteOfDay()
        case MINUTE_OF_HOUR => _chronology.minuteOfHour()
        case SECOND_OF_DAY => _chronology.secondOfDay()
        case SECOND_OF_MINUTE => _chronology.secondOfMinute()
        case MILLIS_OF_DAY => _chronology.millisOfDay()
        case MILLIS_OF_SECOND => _chronology.millisOfSecond()
        case _ => throw new InternalError()
      }
    }

    private def readResolve(): AnyRef = iOrdinal match {
      case ERA => ERA_TYPE
      case YEAR_OF_ERA => YEAR_OF_ERA_TYPE
      case CENTURY_OF_ERA => CENTURY_OF_ERA_TYPE
      case YEAR_OF_CENTURY => YEAR_OF_CENTURY_TYPE
      case YEAR => YEAR_TYPE
      case DAY_OF_YEAR => DAY_OF_YEAR_TYPE
      case MONTH_OF_YEAR => MONTH_OF_YEAR_TYPE
      case DAY_OF_MONTH => DAY_OF_MONTH_TYPE
      case WEEKYEAR_OF_CENTURY => WEEKYEAR_OF_CENTURY_TYPE
      case WEEKYEAR => WEEKYEAR_TYPE
      case WEEK_OF_WEEKYEAR => WEEK_OF_WEEKYEAR_TYPE
      case DAY_OF_WEEK => DAY_OF_WEEK_TYPE
      case HALFDAY_OF_DAY => HALFDAY_OF_DAY_TYPE
      case HOUR_OF_HALFDAY => HOUR_OF_HALFDAY_TYPE
      case CLOCKHOUR_OF_HALFDAY => CLOCKHOUR_OF_HALFDAY_TYPE
      case CLOCKHOUR_OF_DAY => CLOCKHOUR_OF_DAY_TYPE
      case HOUR_OF_DAY => HOUR_OF_DAY_TYPE
      case MINUTE_OF_DAY => MINUTE_OF_DAY_TYPE
      case MINUTE_OF_HOUR => MINUTE_OF_HOUR_TYPE
      case SECOND_OF_DAY => SECOND_OF_DAY_TYPE
      case SECOND_OF_MINUTE => SECOND_OF_MINUTE_TYPE
      case MILLIS_OF_DAY => MILLIS_OF_DAY_TYPE
      case MILLIS_OF_SECOND => MILLIS_OF_SECOND_TYPE
      case _ => this
    }
  }
}

@SerialVersionUID(-42615285973990L)
abstract class DateTimeFieldType protected (name: String) extends Serializable() {

  private val iName: String = name
  
  def getName(): String = iName

  def getDurationType(): DurationFieldType

  def getRangeDurationType(): DurationFieldType

  def getField(chronology: Chronology): DateTimeField

  def isSupported(chronology: Chronology): Boolean = getField(chronology).isSupported

  override def toString(): String = getName
}
