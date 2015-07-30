package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeUtils, Period, ReadWritableInterval, ReadWritablePeriod}

object NullConverter {

  val INSTANCE = new NullConverter()
}

class NullConverter extends AbstractConverter with InstantConverter with PartialConverter with DurationConverter with PeriodConverter with IntervalConverter {

  def getDurationMillis(`object`: AnyRef): Long = 0L

  def setInto(duration: ReadWritablePeriod, `object`: AnyRef, chrono: Chronology) {
    duration.setPeriod(null.asInstanceOf[Period])
  }

  def setInto(writableInterval: ReadWritableInterval, `object`: AnyRef, chrono: Chronology) {
    writableInterval.setChronology(chrono)
    val now = DateTimeUtils.currentTimeMillis()
    writableInterval.setInterval(now, now)
  }

  def getSupportedType(): Class[_] = null
}
