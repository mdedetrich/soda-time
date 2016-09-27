package org.joda.time.convert

import org.joda.time.{
  Chronology,
  PeriodType,
  ReadWritablePeriod,
  ReadablePeriod
}

object ReadablePeriodConverter {

  val INSTANCE = new ReadablePeriodConverter()
}

class ReadablePeriodConverter protected ()
    extends AbstractConverter()
    with PeriodConverter {

  def setInto(duration: ReadWritablePeriod,
              `object`: AnyRef,
              chrono: Chronology) {
    duration.setPeriod(`object`.asInstanceOf[ReadablePeriod])
  }

  override def getPeriodType(`object`: AnyRef): PeriodType = {
    val period = `object`.asInstanceOf[ReadablePeriod]
    period.getPeriodType
  }

  def getSupportedType(): Class[_] = classOf[ReadablePeriod]
}
