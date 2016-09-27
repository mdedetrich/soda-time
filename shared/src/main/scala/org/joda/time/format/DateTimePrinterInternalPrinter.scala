package org.joda.time.format

import java.io.Writer
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeZone
import org.joda.time.ReadablePartial
import scala.beans.BeanProperty

object DateTimePrinterInternalPrinter {

  def of(underlying: DateTimePrinter): InternalPrinter = {
    if (underlying.isInstanceOf[InternalPrinterDateTimePrinter]) {
      return underlying.asInstanceOf[InternalPrinter]
    }
    if (underlying == null) {
      return null
    }
    new DateTimePrinterInternalPrinter(underlying)
  }
}

class DateTimePrinterInternalPrinter private (
    @BeanProperty val underlying: DateTimePrinter)
    extends InternalPrinter {

  def estimatePrintedLength(): Int = underlying.estimatePrintedLength()

  def printTo(appendable: Appendable,
              instant: Long,
              chrono: Chronology,
              displayOffset: Int,
              displayZone: DateTimeZone,
              locale: Locale) {
    if (appendable.isInstanceOf[StringBuffer]) {
      val buf = appendable.asInstanceOf[StringBuffer]
      underlying
        .printTo(buf, instant, chrono, displayOffset, displayZone, locale)
    }
    if (appendable.isInstanceOf[Writer]) {
      val out = appendable.asInstanceOf[Writer]
      underlying
        .printTo(out, instant, chrono, displayOffset, displayZone, locale)
    }
    val buf = new StringBuffer(estimatePrintedLength())
    underlying
      .printTo(buf, instant, chrono, displayOffset, displayZone, locale)
    appendable.append(buf)
  }

  def printTo(appendable: Appendable,
              partial: ReadablePartial,
              locale: Locale) {
    if (appendable.isInstanceOf[StringBuffer]) {
      val buf = appendable.asInstanceOf[StringBuffer]
      underlying.printTo(buf, partial, locale)
    }
    if (appendable.isInstanceOf[Writer]) {
      val out = appendable.asInstanceOf[Writer]
      underlying.printTo(out, partial, locale)
    }
    val buf = new StringBuffer(estimatePrintedLength())
    underlying.printTo(buf, partial, locale)
    appendable.append(buf)
  }
}
