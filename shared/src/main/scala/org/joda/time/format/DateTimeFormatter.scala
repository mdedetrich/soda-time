package org.joda.time.format

import java.io.IOException
import java.io.Writer
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.LocalDate
import org.joda.time.LocalDateTime
import org.joda.time.LocalTime
import org.joda.time.MutableDateTime
import org.joda.time.ReadWritableInstant
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial

class DateTimeFormatter(private val iPrinter: InternalPrinter,
                        private val iParser: InternalParser) {

  private var iLocale: Locale = null
  private var iOffsetParsed: Boolean = false
  private var iChrono: Chronology = null
  private var iZone: DateTimeZone = null
  private var iPivotYear: Integer = null
  private var iDefaultYear: Int = 2000

  def this(printer: DateTimePrinter, parser: DateTimeParser) {
    this(DateTimePrinterInternalPrinter.of(printer),
         DateTimeParserInternalParser.of(parser))
  }

  private def this(printer: InternalPrinter,
                   parser: InternalParser,
                   locale: Locale,
                   offsetParsed: Boolean,
                   chrono: Chronology,
                   zone: DateTimeZone,
                   pivotYear: Integer,
                   defaultYear: Int) {
    this(printer, parser)
    iLocale = locale
    iOffsetParsed = offsetParsed
    iChrono = chrono
    iZone = zone
    iPivotYear = pivotYear
    iDefaultYear = defaultYear
  }

  def isPrinter(): Boolean = (iPrinter != null)

  def getPrinter(): DateTimePrinter = {
    InternalPrinterDateTimePrinter.of(iPrinter)
  }

  def getPrinter0(): InternalPrinter = iPrinter

  def isParser(): Boolean = (iParser != null)

  def getParser(): DateTimeParser = {
    InternalParserDateTimeParser.of(iParser)
  }

  def getParser0(): InternalParser = iParser

  def withLocale(locale: Locale): DateTimeFormatter = {
    if (locale == getLocale || (locale != null && locale == getLocale)) {
      return this
    }
    new DateTimeFormatter(iPrinter,
                          iParser,
                          locale,
                          iOffsetParsed,
                          iChrono,
                          iZone,
                          iPivotYear,
                          iDefaultYear)
  }

  def getLocale(): Locale = iLocale

  def withOffsetParsed(): DateTimeFormatter = {
    if (iOffsetParsed == true) {
      return this
    }
    new DateTimeFormatter(iPrinter,
                          iParser,
                          iLocale,
                          true,
                          iChrono,
                          null,
                          iPivotYear,
                          iDefaultYear)
  }

  def isOffsetParsed(): Boolean = iOffsetParsed

  def withChronology(chrono: Chronology): DateTimeFormatter = {
    if (iChrono == chrono) {
      return this
    }
    new DateTimeFormatter(iPrinter,
                          iParser,
                          iLocale,
                          iOffsetParsed,
                          chrono,
                          iZone,
                          iPivotYear,
                          iDefaultYear)
  }

  def getChronology(): Chronology = iChrono

  @Deprecated
  def getChronolgy(): Chronology = iChrono

  def withZoneUTC(): DateTimeFormatter = withZone(DateTimeZone.UTC)

  def withZone(zone: DateTimeZone): DateTimeFormatter = {
    if (iZone == zone) {
      return this
    }
    new DateTimeFormatter(iPrinter,
                          iParser,
                          iLocale,
                          false,
                          iChrono,
                          zone,
                          iPivotYear,
                          iDefaultYear)
  }

  def getZone(): DateTimeZone = iZone

  def withPivotYear(pivotYear: Integer): DateTimeFormatter = {
    if (iPivotYear == pivotYear || (iPivotYear != null && iPivotYear == pivotYear)) {
      return this
    }
    new DateTimeFormatter(iPrinter,
                          iParser,
                          iLocale,
                          iOffsetParsed,
                          iChrono,
                          iZone,
                          pivotYear,
                          iDefaultYear)
  }

  def withPivotYear(pivotYear: Int): DateTimeFormatter = {
    withPivotYear(Integer.valueOf(pivotYear))
  }

  def getPivotYear(): Integer = iPivotYear

  def withDefaultYear(defaultYear: Int): DateTimeFormatter = {
    new DateTimeFormatter(iPrinter,
                          iParser,
                          iLocale,
                          iOffsetParsed,
                          iChrono,
                          iZone,
                          iPivotYear,
                          defaultYear)
  }

  def getDefaultYear(): Int = iDefaultYear

  def printTo(buf: StringBuffer, instant: ReadableInstant) {
    try {
      printTo(buf.asInstanceOf[Appendable], instant)
    } catch {
      case ex: IOException =>
    }
  }

  def printTo(out: Writer, instant: ReadableInstant) {
    printTo(out.asInstanceOf[Appendable], instant)
  }

  def printTo(appendable: Appendable, instant: ReadableInstant) {
    val millis = DateTimeUtils.getInstantMillis(instant)
    val chrono = DateTimeUtils.getInstantChronology(instant)
    printTo(appendable, millis, chrono)
  }

  def printTo(buf: StringBuffer, instant: Long) {
    try {
      printTo(buf.asInstanceOf[Appendable], instant)
    } catch {
      case ex: IOException =>
    }
  }

  def printTo(out: Writer, instant: Long) {
    printTo(out.asInstanceOf[Appendable], instant)
  }

  def printTo(appendable: Appendable, instant: Long) {
    printTo(appendable, instant, null)
  }

  def printTo(buf: StringBuffer, partial: ReadablePartial) {
    try {
      printTo(buf.asInstanceOf[Appendable], partial)
    } catch {
      case ex: IOException =>
    }
  }

  def printTo(out: Writer, partial: ReadablePartial) {
    printTo(out.asInstanceOf[Appendable], partial)
  }

  def printTo(appendable: Appendable, partial: ReadablePartial) {
    val printer = requirePrinter()
    if (partial == null) {
      throw new IllegalArgumentException("The partial must not be null")
    }
    printer.printTo(appendable, partial, iLocale)
  }

  def print(instant: ReadableInstant): String = {
    val buf = new StringBuilder(requirePrinter().estimatePrintedLength())
    try {
      printTo(buf.asInstanceOf[Appendable], instant)
    } catch {
      case ex: IOException =>
    }
    buf.toString
  }

  def print(instant: Long): String = {
    val buf = new StringBuilder(requirePrinter().estimatePrintedLength())
    try {
      printTo(buf.asInstanceOf[Appendable], instant)
    } catch {
      case ex: IOException =>
    }
    buf.toString
  }

  def print(partial: ReadablePartial): String = {
    val buf = new StringBuilder(requirePrinter().estimatePrintedLength())
    try {
      printTo(buf.asInstanceOf[Appendable], partial)
    } catch {
      case ex: IOException =>
    }
    buf.toString
  }

  private def printTo(appendable: Appendable,
                      instant: Long,
                      chrono: Chronology) {
    var _chrono = chrono
    val printer = requirePrinter()
    _chrono = selectChronology(_chrono)
    var zone = _chrono.getZone
    var offset = zone.getOffset(instant)
    var adjustedInstant = instant + offset
    if ((instant ^ adjustedInstant) < 0 && (instant ^ offset) >= 0) {
      zone = DateTimeZone.UTC
      offset = 0
      adjustedInstant = instant
    }
    printer.printTo(appendable,
                    adjustedInstant,
                    _chrono.withUTC(),
                    offset,
                    zone,
                    iLocale)
  }

  private def requirePrinter(): InternalPrinter = {
    val printer = iPrinter
    if (printer == null) {
      throw new UnsupportedOperationException("Printing not supported")
    }
    printer
  }

  def parseInto(instant: ReadWritableInstant,
                text: String,
                position: Int): Int = {
    val parser = requireParser()
    if (instant == null) {
      throw new IllegalArgumentException("Instant must not be null")
    }
    val instantMillis = instant.getMillis
    var chrono = instant.getChronology
    val defaultYear =
      DateTimeUtils.getChronology(chrono).year().get(instantMillis)
    val instantLocal = instantMillis + chrono.getZone.getOffset(instantMillis)
    chrono = selectChronology(chrono)
    val bucket = new DateTimeParserBucket(instantLocal,
                                          chrono,
                                          iLocale,
                                          iPivotYear,
                                          defaultYear)
    val newPos = parser.parseInto(bucket, text, position)
    instant.setMillis(bucket.computeMillis(false, text))
    if (iOffsetParsed && bucket.getOffsetInteger != null) {
      val parsedOffset = bucket.getOffsetInteger
      val parsedZone = DateTimeZone.forOffsetMillis(parsedOffset)
      chrono = chrono.withZone(parsedZone)
    } else if (bucket.getZone != null) {
      chrono = chrono.withZone(bucket.getZone)
    }
    instant.setChronology(chrono)
    if (iZone != null) {
      instant.setZone(iZone)
    }
    newPos
  }

  def parseMillis(text: String): Long = {
    val parser = requireParser()
    val chrono = selectChronology(iChrono)
    val bucket =
      new DateTimeParserBucket(0, chrono, iLocale, iPivotYear, iDefaultYear)
    bucket.doParseMillis(parser, text)
  }

  def parseLocalDate(text: String): LocalDate =
    parseLocalDateTime(text).toLocalDate()

  def parseLocalTime(text: String): LocalTime =
    parseLocalDateTime(text).toLocalTime()

  def parseLocalDateTime(text: String): LocalDateTime = {
    val parser = requireParser()
    var chrono = selectChronology(null).withUTC()
    val bucket =
      new DateTimeParserBucket(0, chrono, iLocale, iPivotYear, iDefaultYear)
    var newPos = parser.parseInto(bucket, text, 0)
    if (newPos >= 0) {
      if (newPos >= text.length) {
        val millis = bucket.computeMillis(true, text)
        if (bucket.getOffsetInteger != null) {
          val parsedOffset = bucket.getOffsetInteger
          val parsedZone = DateTimeZone.forOffsetMillis(parsedOffset)
          chrono = chrono.withZone(parsedZone)
        } else if (bucket.getZone != null) {
          chrono = chrono.withZone(bucket.getZone)
        }
        return new LocalDateTime(millis, chrono)
      }
    } else {
      newPos = ~newPos
    }
    throw new IllegalArgumentException(
      FormatUtils.createErrorMessage(text, newPos))
  }

  def parseDateTime(text: String): DateTime = {
    val parser = requireParser()
    var chrono = selectChronology(null)
    val bucket =
      new DateTimeParserBucket(0, chrono, iLocale, iPivotYear, iDefaultYear)
    var newPos = parser.parseInto(bucket, text, 0)
    if (newPos >= 0) {
      if (newPos >= text.length) {
        val millis = bucket.computeMillis(true, text)
        if (iOffsetParsed && bucket.getOffsetInteger != null) {
          val parsedOffset = bucket.getOffsetInteger
          val parsedZone = DateTimeZone.forOffsetMillis(parsedOffset)
          chrono = chrono.withZone(parsedZone)
        } else if (bucket.getZone != null) {
          chrono = chrono.withZone(bucket.getZone)
        }
        var dt = new DateTime(millis, chrono)
        if (iZone != null) {
          dt = dt.withZone(iZone)
        }
        return dt
      }
    } else {
      newPos = ~newPos
    }
    throw new IllegalArgumentException(
      FormatUtils.createErrorMessage(text, newPos))
  }

  def parseMutableDateTime(text: String): MutableDateTime = {
    val parser = requireParser()
    var chrono = selectChronology(null)
    val bucket =
      new DateTimeParserBucket(0, chrono, iLocale, iPivotYear, iDefaultYear)
    var newPos = parser.parseInto(bucket, text, 0)
    if (newPos >= 0) {
      if (newPos >= text.length) {
        val millis = bucket.computeMillis(true, text)
        if (iOffsetParsed && bucket.getOffsetInteger != null) {
          val parsedOffset = bucket.getOffsetInteger
          val parsedZone = DateTimeZone.forOffsetMillis(parsedOffset)
          chrono = chrono.withZone(parsedZone)
        } else if (bucket.getZone != null) {
          chrono = chrono.withZone(bucket.getZone)
        }
        val dt = new MutableDateTime(millis, chrono)
        if (iZone != null) {
          dt.setZone(iZone)
        }
        return dt
      }
    } else {
      newPos = ~newPos
    }
    throw new IllegalArgumentException(
      FormatUtils.createErrorMessage(text, newPos))
  }

  private def requireParser(): InternalParser = {
    val parser = iParser
    if (parser == null) {
      throw new UnsupportedOperationException("Parsing not supported")
    }
    parser
  }

  private def selectChronology(chrono: Chronology): Chronology = {
    var _chrono = chrono
    _chrono = DateTimeUtils.getChronology(_chrono)
    if (iChrono != null) {
      _chrono = iChrono
    }
    if (iZone != null) {
      _chrono = _chrono.withZone(iZone)
    }
    _chrono
  }
}
