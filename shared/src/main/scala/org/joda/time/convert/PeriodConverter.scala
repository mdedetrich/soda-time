package org.joda.time.convert

import org.joda.time.{Chronology, PeriodType, ReadWritablePeriod}

trait PeriodConverter extends Converter {

  def setInto(period: ReadWritablePeriod, `object`: AnyRef, chrono: Chronology): Unit

  def getPeriodType(`object`: AnyRef): PeriodType
}
