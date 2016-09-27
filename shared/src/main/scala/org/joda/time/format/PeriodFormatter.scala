package org.joda.time.format

import java.io.Writer
import java.util.Locale
import org.joda.time.MutablePeriod
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.ReadWritablePeriod
import org.joda.time.ReadablePeriod

class PeriodFormatter {

  private var iLocale: Locale = null
  private var iParseType: PeriodType = null
  private var iPrinter: PeriodPrinter = null
  private var iParser: PeriodParser = null

  def this(printer: PeriodPrinter, parser: PeriodParser) {
    this()
    iPrinter = printer
    iParser = parser
    iLocale = null
    iParseType = null
  }

  def this(printer: PeriodPrinter,
           parser: PeriodParser,
           locale: Locale,
           `type`: PeriodType) {
    this()
    iPrinter = printer
    iParser = parser
    iLocale = locale
    iParseType = `type`
  }

  def isPrinter(): Boolean = iPrinter != null

  def getPrinter(): PeriodPrinter = iPrinter

  def isParser(): Boolean = iParser != null

  def getParser(): PeriodParser = iParser

  def withLocale(locale: Locale): PeriodFormatter = {
    if (locale == getLocale || (locale != null && locale == getLocale)) {
      return this
    }
    new PeriodFormatter(iPrinter, iParser, locale, iParseType)
  }

  def getLocale(): Locale = iLocale

  def withParseType(`type`: PeriodType): PeriodFormatter = {
    if (`type` == iParseType) {
      return this
    }
    new PeriodFormatter(iPrinter, iParser, iLocale, `type`)
  }

  def getParseType(): PeriodType = iParseType

  def printTo(buf: StringBuffer, period: ReadablePeriod) {
    checkPrinter()
    checkPeriod(period)
    getPrinter.printTo(buf, period, iLocale)
  }

  def printTo(out: Writer, period: ReadablePeriod) {
    checkPrinter()
    checkPeriod(period)
    getPrinter.printTo(out, period, iLocale)
  }

  def print(period: ReadablePeriod): String = {
    checkPrinter()
    checkPeriod(period)
    val printer = getPrinter
    val buf = new StringBuffer(printer.calculatePrintedLength(period, iLocale))
    printer.printTo(buf, period, iLocale)
    buf.toString
  }

  private def checkPrinter() {
    if (iPrinter == null) {
      throw new UnsupportedOperationException("Printing not supported")
    }
  }

  private def checkPeriod(period: ReadablePeriod) {
    if (period == null) {
      throw new IllegalArgumentException("Period must not be null")
    }
  }

  def parseInto(period: ReadWritablePeriod, text: String, position: Int): Int = {
    checkParser()
    checkPeriod(period)
    getParser.parseInto(period, text, position, iLocale)
  }

  def parsePeriod(text: String): Period = {
    checkParser()
    parseMutablePeriod(text).toPeriod()
  }

  def parseMutablePeriod(text: String): MutablePeriod = {
    checkParser()
    val period = new MutablePeriod(0, iParseType)
    var newPos = getParser.parseInto(period, text, 0, iLocale)
    if (newPos >= 0) {
      if (newPos >= text.length) {
        return period
      }
    } else {
      newPos = ~newPos
    }
    throw new IllegalArgumentException(
      FormatUtils.createErrorMessage(text, newPos))
  }

  private def checkParser() {
    if (iParser == null) {
      throw new UnsupportedOperationException("Parsing not supported")
    }
  }
}
