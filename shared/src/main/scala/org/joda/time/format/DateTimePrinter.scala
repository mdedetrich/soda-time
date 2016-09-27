package org.joda.time.format

import java.io.Writer
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeZone
import org.joda.time.ReadablePartial

trait DateTimePrinter {

  def estimatePrintedLength(): Int

  def printTo(buf: StringBuffer,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale): Unit

  def printTo(out: Writer,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale): Unit

  def printTo(buf: StringBuffer,
              partial: ReadablePartial,
              locale: Locale): Unit

  def printTo(out: Writer, partial: ReadablePartial, locale: Locale): Unit
}
