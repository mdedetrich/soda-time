package org.joda.time.format

import java.io.IOException
import java.io.Writer
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeZone
import org.joda.time.ReadablePartial

object InternalPrinterDateTimePrinter {

  def of(underlying: InternalPrinter): DateTimePrinter = {
    if (underlying.isInstanceOf[DateTimePrinterInternalPrinter]) {
      return underlying.asInstanceOf[DateTimePrinterInternalPrinter]
        .getUnderlying
    }
    if (underlying.isInstanceOf[DateTimePrinter]) {
      return underlying.asInstanceOf[DateTimePrinter]
    }
    if (underlying == null) {
      return null
    }
    new InternalPrinterDateTimePrinter(underlying)
  }
}

class InternalPrinterDateTimePrinter private (private val underlying: InternalPrinter)
  extends DateTimePrinter with InternalPrinter {

  def estimatePrintedLength(): Int = underlying.estimatePrintedLength()

  def printTo(buf: StringBuffer,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale) {
    try {
      underlying.printTo(buf, instant, chrono, displayOffset, displayZone, locale)
    } catch {
      case ex: IOException =>
    }
  }

  def printTo(out: Writer,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale) {
    underlying.printTo(out, instant, chrono, displayOffset, displayZone, locale)
  }

  def printTo(appendable: Appendable,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale) {
    underlying.printTo(appendable, instant, chrono, displayOffset, displayZone, locale)
  }

  def printTo(buf: StringBuffer, partial: ReadablePartial, locale: Locale) {
    try {
      underlying.printTo(buf, partial, locale)
    } catch {
      case ex: IOException =>
    }
  }

  def printTo(out: Writer, partial: ReadablePartial, locale: Locale) {
    underlying.printTo(out, partial, locale)
  }

  def printTo(appendable: Appendable, partial: ReadablePartial, locale: Locale) {
    underlying.printTo(appendable, partial, locale)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == this) {
      return true
    }
    if (obj.isInstanceOf[InternalPrinterDateTimePrinter]) {
      val other = obj.asInstanceOf[InternalPrinterDateTimePrinter]
      return underlying == other.underlying
    }
    false
  }
}
