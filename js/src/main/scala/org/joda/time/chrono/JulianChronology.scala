package org.joda.time.chrono

import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeZone
import org.joda.time.IllegalFieldValueException
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.SkipDateTimeField
import JulianChronology._
import scala.scalajs.js

object JulianChronology {

  private val MILLIS_PER_YEAR =
    (365.25 * DateTimeConstants.MILLIS_PER_DAY).toLong
  private val MILLIS_PER_MONTH =
    (365.25 * DateTimeConstants.MILLIS_PER_DAY / 12).toLong
  private val MIN_YEAR = -292269054
  private val MAX_YEAR = 292272992
  private val INSTANCE_UTC = getInstance(DateTimeZone.UTC)
  private val cCache =
    new collection.mutable.HashMap[DateTimeZone, js.Array[JulianChronology]]()

  def adjustYearForSet(year: Int): Int = {
    var _year: Int = year
    if (_year <= 0) {
      if (_year == 0) {
        throw IllegalFieldValueException
          .create(DateTimeFieldType.year(), Integer.valueOf(_year), null, null)
      }
      _year += 1
    }
    _year
  }

  def getInstanceUTC(): JulianChronology = INSTANCE_UTC

  def getInstance(): JulianChronology = getInstance(DateTimeZone.getDefault, 4)

  def getInstance(zone: DateTimeZone): JulianChronology = getInstance(zone, 4)

  def getInstance(zone: DateTimeZone,
                  minDaysInFirstWeek: Int): JulianChronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    var chrono: JulianChronology = null
    var chronos = cCache.get(_zone).orNull
    if (chronos == null) {
      chronos = js.Array[JulianChronology]()
      val oldChronos = if (cCache.get(_zone).isEmpty) {
        cCache(_zone) = chronos
        chronos
      } else {
        chronos
      }
      if (oldChronos != null) {
        chronos = oldChronos
      }
    }
    chrono = chronos(minDaysInFirstWeek - 1)
    if (chrono == null) {
      chronos.synchronized {
        chrono = chronos(minDaysInFirstWeek - 1)
        if (chrono == null) {
          if (_zone == DateTimeZone.UTC) {
            chrono = new JulianChronology(null, null, minDaysInFirstWeek)
          } else {
            chrono = getInstance(DateTimeZone.UTC, minDaysInFirstWeek)
            chrono =
              new JulianChronology(ZonedChronology.getInstance(chrono, _zone),
                                   null,
                                   minDaysInFirstWeek)
          }
          chronos(minDaysInFirstWeek - 1) = chrono
        }
      }
    }
    chrono
  }
}

@SerialVersionUID(-8731039522547897247L)
class JulianChronology(base: Chronology,
                       param: AnyRef,
                       minDaysInFirstWeek: Int)
    extends BasicGJChronology(base, param, minDaysInFirstWeek) {

  private def readResolve(): AnyRef = {
    val base = getBase
    var minDays = getMinimumDaysInFirstWeek
    minDays = if (minDays == 0) 4 else minDays
    if (base == null) getInstance(DateTimeZone.UTC, minDays)
    else getInstance(base.getZone, minDays)
  }

  def withUTC(): Chronology = INSTANCE_UTC

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    getInstance(_zone)
  }

  override def getDateMidnightMillis(year: Int,
                                     monthOfYear: Int,
                                     dayOfMonth: Int): Long = {
    super
      .getDateMidnightMillis(adjustYearForSet(year), monthOfYear, dayOfMonth)
  }

  def isLeapYear(year: Int): Boolean = (year & 3) == 0

  def calculateFirstDayOfYearMillis(year: Int): Long = {
    val relativeYear = year - 1968
    var leapYears: Int = 0
    if (relativeYear <= 0) {
      leapYears = (relativeYear + 3) >> 2
    } else {
      leapYears = relativeYear >> 2
      if (!isLeapYear(year)) {
        leapYears += 1
      }
    }
    val millis = (relativeYear * 365L + leapYears) * DateTimeConstants.MILLIS_PER_DAY.toLong
    millis - (366L + 352) * DateTimeConstants.MILLIS_PER_DAY
  }

  def getMinYear(): Int = MIN_YEAR

  def getMaxYear(): Int = MAX_YEAR

  def getAverageMillisPerYear(): Long = MILLIS_PER_YEAR

  def getAverageMillisPerYearDividedByTwo(): Long = MILLIS_PER_YEAR / 2

  def getAverageMillisPerMonth(): Long = MILLIS_PER_MONTH

  def getApproxMillisAtEpochDividedByTwo(): Long = {
    (1969L * MILLIS_PER_YEAR + 352L * DateTimeConstants.MILLIS_PER_DAY) / 2
  }

  override protected def assemble(fields: Fields) {
    if (getBase == null) {
      super.assemble(fields)
      fields.year = new SkipDateTimeField(this, fields.year)
      fields.weekyear = new SkipDateTimeField(this, fields.weekyear)
    }
  }
}
