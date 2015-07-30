package org.joda.time.convert

import org.joda.time.chrono.ISOChronology
import org.joda.time.{Chronology, DateTimeUtils, DateTimeZone, ReadableInstant}

object ReadableInstantConverter {

  val INSTANCE = new ReadableInstantConverter()
}

class ReadableInstantConverter extends AbstractConverter() with InstantConverter with PartialConverter {

  override def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology = {
    var chrono = `object`.asInstanceOf[ReadableInstant].getChronology
    if (chrono == null) {
      return ISOChronology.getInstance(zone)
    }
    val chronoZone = chrono.getZone
    if (chronoZone != zone) {
      chrono = chrono.withZone(zone)
      if (chrono == null) {
        return ISOChronology.getInstance(zone)
      }
    }
    chrono
  }

  override def getChronology(`object`: AnyRef, chrono: Chronology): Chronology = {
    var _chrono: Chronology = chrono
    if (_chrono == null) {
      _chrono = `object`.asInstanceOf[ReadableInstant].getChronology
      _chrono = DateTimeUtils.getChronology(_chrono)
    }
    _chrono
  }

  override def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = {
    `object`.asInstanceOf[ReadableInstant].getMillis
  }

  def getSupportedType(): Class[_] = classOf[ReadableInstant]
}
