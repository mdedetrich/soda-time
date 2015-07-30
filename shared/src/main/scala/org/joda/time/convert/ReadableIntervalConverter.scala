package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeUtils, ReadWritableInterval, ReadWritablePeriod, ReadableInterval}

object ReadableIntervalConverter {
  val INSTANCE = new ReadableIntervalConverter()
}

class ReadableIntervalConverter protected () extends AbstractConverter() with IntervalConverter with DurationConverter with PeriodConverter {

  def getDurationMillis(`object`: AnyRef): Long = {
    (`object`.asInstanceOf[ReadableInterval]).toDurationMillis()
  }

  def setInto(writablePeriod: ReadWritablePeriod, `object`: AnyRef, chrono: Chronology) {
    var _chrono: Chronology = chrono
    val interval = `object`.asInstanceOf[ReadableInterval]
    _chrono = if (_chrono != null) _chrono else DateTimeUtils.getIntervalChronology(interval)
    val start = interval.getStartMillis
    val end = interval.getEndMillis
    val values = _chrono.get(writablePeriod, start, end)
    for (i <- 0 until values.length) {
      writablePeriod.setValue(i, values(i))
    }
  }

  override def isReadableInterval(`object`: AnyRef, chrono: Chronology): Boolean = true

  def setInto(writableInterval: ReadWritableInterval, `object`: AnyRef, chrono: Chronology) {
    val input = `object`.asInstanceOf[ReadableInterval]
    writableInterval.setInterval(input)
    if (chrono != null) {
      writableInterval.setChronology(chrono)
    } else {
      writableInterval.setChronology(input.getChronology)
    }
  }

  def getSupportedType(): Class[_] = classOf[ReadableInterval]
}
