package org.joda.time.chrono

import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeZone
import GregorianChronology._
import org.joda.time.chrono.AssembledChronology.Fields
import scala.scalajs.js

object GregorianChronology {

  private final val cCache =
    new collection.mutable.HashMap[DateTimeZone,
                                   js.Array[GregorianChronology]]()
  private final val MILLIS_PER_YEAR =
    (365.2425 * DateTimeConstants.MILLIS_PER_DAY).toLong
  private final val MILLIS_PER_MONTH =
    (365.2425 * DateTimeConstants.MILLIS_PER_DAY / 12).toLong
  private final val DAYS_0000_TO_1970 = 719527
  private final val MIN_YEAR = -292275054
  private final val MAX_YEAR = 292278993

  private final val INSTANCE_UTC = getInstance(DateTimeZone.UTC)

  def getInstanceUTC(): GregorianChronology = INSTANCE_UTC

  def getInstance(): GregorianChronology =
    getInstance(DateTimeZone.getDefault, 4)

  def getInstance(zone: DateTimeZone): GregorianChronology =
    getInstance(zone, 4)

  def getInstance(zone: DateTimeZone,
                  minDaysInFirstWeek: Int): GregorianChronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    var chrono: GregorianChronology = null
    var chronos = cCache.get(_zone).orNull
    if (chronos == null) {
      chronos = js.Array[GregorianChronology]()
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
            chrono = new GregorianChronology(null, null, minDaysInFirstWeek)
          } else {
            chrono = getInstance(DateTimeZone.UTC, minDaysInFirstWeek)
            chrono = new GregorianChronology(
              ZonedChronology.getInstance(chrono, _zone),
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

@SerialVersionUID(-861407383323710522L)
class GregorianChronology private (base: Chronology,
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
    var _zone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    getInstance(_zone)
  }

  override protected def assemble(fields: Fields) {
    if (getBase == null) {
      super.assemble(fields)
    }
  }

  def isLeapYear(year: Int): Boolean = {
    ((year & 3) == 0) && ((year % 100) != 0 || (year % 400) == 0)
  }

  def calculateFirstDayOfYearMillis(year: Int): Long = {
    var leapYears = year / 100
    if (year < 0) {
      leapYears = ((year + 3) >> 2) - leapYears + ((leapYears + 3) >> 2) -
          1
    } else {
      leapYears = (year >> 2) - leapYears + (leapYears >> 2)
      if (isLeapYear(year)) {
        leapYears -= 1
      }
    }
    (year * 365L + (leapYears - DAYS_0000_TO_1970)) * DateTimeConstants.MILLIS_PER_DAY
  }

  def getMinYear(): Int = MIN_YEAR

  def getMaxYear(): Int = MAX_YEAR

  def getAverageMillisPerYear(): Long = MILLIS_PER_YEAR

  def getAverageMillisPerYearDividedByTwo(): Long = MILLIS_PER_YEAR / 2

  def getAverageMillisPerMonth(): Long = MILLIS_PER_MONTH

  def getApproxMillisAtEpochDividedByTwo(): Long =
    (1970L * MILLIS_PER_YEAR) / 2
}
