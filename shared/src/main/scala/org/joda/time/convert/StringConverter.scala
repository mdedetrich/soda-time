package org.joda.time.convert

import org.joda.time.field.FieldUtils
import org.joda.time.format.{
  DateTimeFormatter,
  ISODateTimeFormat,
  ISOPeriodFormat
}
import org.joda.time.{
  Chronology,
  Period,
  ReadWritableInterval,
  ReadWritablePeriod,
  ReadablePartial
}

object StringConverter {
  val INSTANCE = new StringConverter()
}

class StringConverter
    extends AbstractConverter()
    with InstantConverter
    with PartialConverter
    with DurationConverter
    with PeriodConverter
    with IntervalConverter {

  override def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = {
    val str = `object`.asInstanceOf[String]
    val p = ISODateTimeFormat.dateTimeParser()
    p.withChronology(chrono).parseMillis(str)
  }

  override def getPartialValues(fieldSource: ReadablePartial,
                                `object`: AnyRef,
                                chrono: Chronology,
                                parser: DateTimeFormatter): Array[Int] = {
    var _chrono: Chronology = chrono
    if (parser.getZone != null) {
      _chrono = _chrono.withZone(parser.getZone)
    }
    val millis =
      parser.withChronology(_chrono).parseMillis(`object`.asInstanceOf[String])
    _chrono.get(fieldSource, millis)
  }

  def getDurationMillis(`object`: AnyRef): Long = {
    val original = `object`.asInstanceOf[String]
    var str = original
    val len = str.length
    if (len >= 4 && (str.charAt(0) == 'P' || str.charAt(0) == 'p') &&
        (str.charAt(1) == 'T' || str.charAt(1) == 't') &&
        (str.charAt(len - 1) == 'S' || str.charAt(len - 1) == 's')) {} else {
      throw new IllegalArgumentException("Invalid format: \"" + original + '"')
    }
    str = str.substring(2, len - 1)
    var dot = -1
    var negative = false
    for (i <- 0 until str.length) {
      if (str.charAt(i) >= '0' && str.charAt(i) <= '9') {} else if (i == 0 && str
                                                                      .charAt(
                                                                        0) == '-') {
        negative = true
      } else if (i > (if (negative) 1 else 0) && str.charAt(i) == '.' &&
                 dot == -1) {
        dot = i
      } else {
        throw new IllegalArgumentException(
          "Invalid format: \"" + original + '"')
      }
    }
    var millis = 0
    var seconds: Long = 0
    val firstDigit = if (negative) 1 else 0
    if (dot > 0) {
      seconds = str.substring(firstDigit, dot).toLong
      str = str.substring(dot + 1)
      if (str.length != 3) {
        str = (str + "000").substring(0, 3)
      }
      millis = Integer.parseInt(str)
    } else
      seconds =
        if (negative) str.substring(firstDigit, str.length).toLong
        else str.toLong
    if (negative) {
      FieldUtils.safeAdd(FieldUtils.safeMultiply(-seconds, 1000), -millis)
    } else {
      FieldUtils.safeAdd(FieldUtils.safeMultiply(seconds, 1000), millis)
    }
  }

  def setInto(period: ReadWritablePeriod,
              `object`: AnyRef,
              chrono: Chronology) {
    val str = `object`.asInstanceOf[String]
    val parser = ISOPeriodFormat.standard()
    period.clear()
    val pos = parser.parseInto(period, str, 0)
    if (pos < str.length) {
      if (pos < 0) {
        parser.withParseType(period.getPeriodType).parseMutablePeriod(str)
      }
      throw new IllegalArgumentException("Invalid format: \"" + str + '"')
    }
  }

  def setInto(writableInterval: ReadWritableInterval,
              `object`: AnyRef,
              chrono: Chronology) {
    var _chrono: Chronology = chrono
    val str = `object`.asInstanceOf[String]
    val separator = str.indexOf('/')
    if (separator < 0) {
      throw new IllegalArgumentException(
        "Format requires a '/' separator: " + str)
    }
    val leftStr = str.substring(0, separator)
    if (leftStr.length <= 0) {
      throw new IllegalArgumentException("Format invalid: " + str)
    }
    val rightStr = str.substring(separator + 1)
    if (rightStr.length <= 0) {
      throw new IllegalArgumentException("Format invalid: " + str)
    }
    var dateTimeParser = ISODateTimeFormat.dateTimeParser()
    dateTimeParser = dateTimeParser.withChronology(_chrono)
    val periodParser = ISOPeriodFormat.standard()
    var startInstant: Long = 0
    var endInstant: Long = 0
    var period: Period = null
    var parsedChrono: Chronology = null
    var c = leftStr.charAt(0)
    if (c == 'P' || c == 'p') {
      period =
        periodParser.withParseType(getPeriodType(leftStr)).parsePeriod(leftStr)
    } else {
      val start = dateTimeParser.parseDateTime(leftStr)
      startInstant = start.getMillis
      parsedChrono = start.getChronology
    }
    c = rightStr.charAt(0)
    if (c == 'P' || c == 'p') {
      if (period != null) {
        throw new IllegalArgumentException(
          "Interval composed of two durations: " + str)
      }
      period = periodParser
        .withParseType(getPeriodType(rightStr))
        .parsePeriod(rightStr)
      _chrono = if (_chrono != null) _chrono else parsedChrono
      endInstant = _chrono.add(period, startInstant, 1)
    } else {
      val end = dateTimeParser.parseDateTime(rightStr)
      endInstant = end.getMillis
      parsedChrono =
        if (parsedChrono != null) parsedChrono else end.getChronology
      _chrono = if (_chrono != null) _chrono else parsedChrono
      if (period != null) {
        startInstant = _chrono.add(period, endInstant, -1)
      }
    }
    writableInterval.setInterval(startInstant, endInstant)
    writableInterval.setChronology(_chrono)
  }

  def getSupportedType(): Class[_] = classOf[String]
}
