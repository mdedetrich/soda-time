package org.joda.time.format

import java.io.Writer
import java.util.Locale
import org.joda.time.ReadablePeriod

trait PeriodPrinter {

  def calculatePrintedLength(period: ReadablePeriod, locale: Locale): Int

  def countFieldsToPrint(period: ReadablePeriod,
                         stopAt: Int,
                         locale: Locale): Int

  def printTo(buf: StringBuffer, period: ReadablePeriod, locale: Locale): Unit

  def printTo(out: Writer, period: ReadablePeriod, locale: Locale): Unit
}
