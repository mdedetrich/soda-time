package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeZone}

trait InstantConverter extends Converter {

  def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology

  def getChronology(`object`: AnyRef, chrono: Chronology): Chronology

  def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long
}
