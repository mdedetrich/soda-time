package org.joda.time.convert

import org.joda.time.{Chronology, ReadWritableInterval}

trait IntervalConverter extends Converter {

  def isReadableInterval(`object`: AnyRef, chrono: Chronology): Boolean

  def setInto(writableInterval: ReadWritableInterval,
              `object`: AnyRef,
              chrono: Chronology): Unit
}
