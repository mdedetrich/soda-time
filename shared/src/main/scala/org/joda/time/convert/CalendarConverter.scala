package org.joda.time.convert

import java.util.{Calendar, GregorianCalendar}

import org.joda.time.chrono.{BuddhistChronology, GJChronology, GregorianChronology, ISOChronology, JulianChronology}
import org.joda.time.{Chronology, DateTimeZone}

object CalendarConverter {
  val INSTANCE = new CalendarConverter()
}

class CalendarConverter extends AbstractConverter with InstantConverter with PartialConverter {

  override def getChronology(`object`: AnyRef, chrono: Chronology): Chronology = {
    if (chrono != null) {
      return chrono
    }
    val cal = `object`.asInstanceOf[Calendar]
    var zone: DateTimeZone = null
    try {
      zone = DateTimeZone.forTimeZone(cal.getTimeZone)
    } catch {
      case ex: IllegalArgumentException => zone = DateTimeZone.getDefault
    }
    getChronology(cal, zone)
  }

  override def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology = {
    if (`object`.getClass.getName.endsWith(".BuddhistCalendar")) {
      BuddhistChronology.getInstance(zone)
    } else if (`object`.isInstanceOf[GregorianCalendar]) {
      val gc = `object`.asInstanceOf[GregorianCalendar]
      val cutover = gc.getGregorianChange.getTime
      if (cutover == Long.MinValue) {
        GregorianChronology.getInstance(zone)
      } else if (cutover == Long.MaxValue) {
        JulianChronology.getInstance(zone)
      } else {
        GJChronology.getInstance(zone, cutover, 4)
      }
    } else {
      ISOChronology.getInstance(zone)
    }
  }

  override def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = {
    val calendar = `object`.asInstanceOf[Calendar]
    calendar.getTime.getTime
  }

  def getSupportedType(): Class[_] = classOf[Calendar]
}
