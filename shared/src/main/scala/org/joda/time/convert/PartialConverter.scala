package org.joda.time.convert

import org.joda.time.{Chronology, DateTimeZone, ReadablePartial}
import org.joda.time.format.DateTimeFormatter

trait PartialConverter extends Converter {

  def getChronology(`object`: AnyRef, zone: DateTimeZone): Chronology

  def getChronology(`object`: AnyRef, chrono: Chronology): Chronology

  def getPartialValues(fieldSource: ReadablePartial,
                       `object`: AnyRef,
                       chrono: Chronology): Array[Int]

  def getPartialValues(fieldSource: ReadablePartial,
                       `object`: AnyRef,
                       chrono: Chronology,
                       parser: DateTimeFormatter): Array[Int]
}
