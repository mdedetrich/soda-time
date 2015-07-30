package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeUtils, DateTimeZone, ReadablePartial}

object ReadablePartialConverter {
  val INSTANCE = new ReadablePartialConverter()
}

class ReadablePartialConverter protected () extends AbstractConverter() with PartialConverter {

  override def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology = {
    getChronology(`object`, null.asInstanceOf[Chronology])
      .withZone(zone)
  }

  override def getChronology(`object`: AnyRef, chrono: Chronology): Chronology = {
    var _chrono: Chronology = chrono
    if (_chrono == null) {
      _chrono = `object`.asInstanceOf[ReadablePartial].getChronology
      _chrono = DateTimeUtils.getChronology(_chrono)
    }
    _chrono
  }

  override def getPartialValues(fieldSource: ReadablePartial, `object`: AnyRef, chrono: Chronology): Array[Int] = {
    val input = `object`.asInstanceOf[ReadablePartial]
    val size = fieldSource.size
    val values = Array.ofDim[Int](size)
    for (i <- 0 until size) {
      values(i) = input.get(fieldSource.getFieldType(i))
    }
    chrono.validate(fieldSource, values)
    values
  }

  def getSupportedType(): Class[_] = classOf[ReadablePartial]
}
