package org.joda.time.format

import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeZone
import org.joda.time.ReadablePartial

trait InternalPrinter {

  def estimatePrintedLength(): Int

  def printTo(appendable: Appendable,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale): Unit

  def printTo(appendable: Appendable, partial: ReadablePartial, locale: Locale): Unit
}
