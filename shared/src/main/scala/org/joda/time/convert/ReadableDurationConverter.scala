package org.joda.time.convert

import org.joda.time.{
  Chronology,
  DateTimeUtils,
  ReadWritablePeriod,
  ReadableDuration
}

object ReadableDurationConverter {
  val INSTANCE = new ReadableDurationConverter()
}

class ReadableDurationConverter
    extends AbstractConverter
    with DurationConverter
    with PeriodConverter {

  def getDurationMillis(`object`: AnyRef): Long = {
    `object`.asInstanceOf[ReadableDuration].getMillis
  }

  def setInto(writablePeriod: ReadWritablePeriod,
              `object`: AnyRef,
              chrono: Chronology) {
    var _chrono: Chronology = chrono
    val dur = `object`.asInstanceOf[ReadableDuration]
    _chrono = DateTimeUtils.getChronology(_chrono)
    val duration = dur.getMillis
    val values = _chrono.get(writablePeriod, duration)
    for (i <- 0 until values.length) {
      writablePeriod.setValue(i, values(i))
    }
  }

  def getSupportedType(): Class[_] = classOf[ReadableDuration]
}
