package org.joda.time

import java.io.Serializable
import DurationFieldType._

object DurationFieldType {

  val ERAS:Byte = 1
  val CENTURIES:Byte = 2
  val WEEKYEARS:Byte = 3
  val YEARS:Byte = 4
  val MONTHS:Byte = 5
  val WEEKS:Byte = 6
  val DAYS:Byte = 7
  val HALFDAYS:Byte = 8
  val HOURS:Byte = 9
  val MINUTES:Byte = 10
  val SECONDS:Byte = 11
  val MILLIS:Byte = 12

  val ERAS_TYPE = new StandardDurationFieldType("eras", ERAS)
  val CENTURIES_TYPE = new StandardDurationFieldType("centuries", CENTURIES)
  val WEEKYEARS_TYPE = new StandardDurationFieldType("weekyears", WEEKYEARS)
  val YEARS_TYPE = new StandardDurationFieldType("years", YEARS)
  val MONTHS_TYPE = new StandardDurationFieldType("months", MONTHS)
  val WEEKS_TYPE = new StandardDurationFieldType("weeks", WEEKS)
  val DAYS_TYPE = new StandardDurationFieldType("days", DAYS)
  val HALFDAYS_TYPE = new StandardDurationFieldType("halfdays", HALFDAYS)
  val HOURS_TYPE = new StandardDurationFieldType("hours", HOURS)
  val MINUTES_TYPE = new StandardDurationFieldType("minutes", MINUTES)
  val SECONDS_TYPE = new StandardDurationFieldType("seconds", SECONDS)
  val MILLIS_TYPE = new StandardDurationFieldType("millis", MILLIS)

  def millis(): DurationFieldType = MILLIS_TYPE

  def seconds(): DurationFieldType = SECONDS_TYPE

  def minutes(): DurationFieldType = MINUTES_TYPE

  def hours(): DurationFieldType = HOURS_TYPE

  def halfdays(): DurationFieldType = HALFDAYS_TYPE

  def days(): DurationFieldType = DAYS_TYPE

  def weeks(): DurationFieldType = WEEKS_TYPE

  def weekyears(): DurationFieldType = WEEKYEARS_TYPE

  def months(): DurationFieldType = MONTHS_TYPE

  def years(): DurationFieldType = YEARS_TYPE

  def centuries(): DurationFieldType = CENTURIES_TYPE

  def eras(): DurationFieldType = ERAS_TYPE

  @SerialVersionUID(31156755687123L)
  class StandardDurationFieldType(name: String, private val iOrdinal: Byte)
    extends DurationFieldType(name) {

    override def equals(obj: Any): Boolean = {
      if (this == obj) {
        return true
      }
      if (obj.isInstanceOf[StandardDurationFieldType]) {
        return iOrdinal ==
          obj.asInstanceOf[StandardDurationFieldType].iOrdinal
      }
      false
    }

    override def hashCode(): Int = 1 << iOrdinal

    def getField(chronology: Chronology): DurationField = {
      var _chronology: Chronology = chronology
      _chronology = DateTimeUtils.getChronology(_chronology)
      iOrdinal match {
        case ERAS => _chronology.eras()
        case CENTURIES => _chronology.centuries()
        case WEEKYEARS => _chronology.weekyears()
        case YEARS => _chronology.years()
        case MONTHS => _chronology.months()
        case WEEKS => _chronology.weeks()
        case DAYS => _chronology.days()
        case HALFDAYS => _chronology.halfdays()
        case HOURS => _chronology.hours()
        case MINUTES => _chronology.minutes()
        case SECONDS => _chronology.seconds()
        case MILLIS => _chronology.millis()
        case _ => throw new InternalError()
      }
    }

    private def readResolve(): AnyRef = iOrdinal match {
      case ERAS => ERAS_TYPE
      case CENTURIES => CENTURIES_TYPE
      case WEEKYEARS => WEEKYEARS_TYPE
      case YEARS => YEARS_TYPE
      case MONTHS => MONTHS_TYPE
      case WEEKS => WEEKS_TYPE
      case DAYS => DAYS_TYPE
      case HALFDAYS => HALFDAYS_TYPE
      case HOURS => HOURS_TYPE
      case MINUTES => MINUTES_TYPE
      case SECONDS => SECONDS_TYPE
      case MILLIS => MILLIS_TYPE
      case _ => this
    }
  }
}

@SerialVersionUID(8765135187319L)
abstract class DurationFieldType protected (private val iName: String) extends Serializable() {

  def getName(): String = iName

  def getField(chronology: Chronology): DurationField

  def isSupported(chronology: Chronology): Boolean = getField(chronology).isSupported

  override def toString(): String = getName
}
