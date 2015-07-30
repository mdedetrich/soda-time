package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeUtils, DateTimeZone, PeriodType, ReadablePartial}
import org.joda.time.chrono.ISOChronology
import org.joda.time.format.DateTimeFormatter

abstract class AbstractConverter protected () extends Converter() {

  def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = DateTimeUtils.currentTimeMillis()

  def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology = ISOChronology.getInstance(zone)

  def getChronology(`object`: AnyRef, chrono: Chronology): Chronology = DateTimeUtils.getChronology(chrono)

  def getPartialValues(fieldSource: ReadablePartial, `object`: AnyRef, chrono: Chronology): Array[Int] = {
    val instant = getInstantMillis(`object`, chrono)
    chrono.get(fieldSource, instant)
  }

  def getPartialValues(fieldSource: ReadablePartial,
                       `object`: AnyRef,
                       chrono: Chronology,
                       parser: DateTimeFormatter): Array[Int] = {
    getPartialValues(fieldSource, `object`, chrono)
  }

  def getPeriodType(`object`: AnyRef): PeriodType = PeriodType.standard()

  def isReadableInterval(`object`: AnyRef, chrono: Chronology): Boolean = false

  override def toString(): String = {
    "Converter[" +
      (if (getSupportedType == null) "null" else getSupportedType.getName) +
      "]"
  }
}
