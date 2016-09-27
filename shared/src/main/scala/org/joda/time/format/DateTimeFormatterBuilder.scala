package org.joda.time.format

import java.util.ArrayList
import java.util.List
import java.util.Locale
import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.MutableDateTime
import org.joda.time.ReadablePartial
import org.joda.time.field.MillisDurationField
import org.joda.time.field.PreciseDateTimeField
import DateTimeFormatterBuilder._
import scala.util.control.Breaks._

object DateTimeFormatterBuilder {

  def appendUnknownString(appendable: Appendable, len: Int) {
    val i = len
    while (i >= 0) {
      appendable.append('�')
    }
  }

  class CharacterLiteral(private val value: Char)
      extends InternalPrinter
      with InternalParser {

    private var iValue: Char = _

    iValue = value

    def estimatePrintedLength(): Int = 1

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      appendable.append(iValue)
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      appendable.append(iValue)
    }

    def estimateParsedLength(): Int = 1

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      if (position >= text.length) {
        return ~position
      }
      var a = text.charAt(position)
      var b = iValue
      if (a != b) {
        a = Character.toUpperCase(a)
        b = Character.toUpperCase(b)
        if (a != b) {
          a = Character.toLowerCase(a)
          b = Character.toLowerCase(b)
          if (a != b) {
            return ~position
          }
        }
      }
      position + 1
    }
  }

  class StringLiteral(value: String)
      extends InternalPrinter()
      with InternalParser {

    var iValue: String = _

    iValue = value

    def estimatePrintedLength(): Int = iValue.length

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      appendable.append(iValue)
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      appendable.append(iValue)
    }

    def estimateParsedLength(): Int = iValue.length

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      if (csStartsWithIgnoreCase(text, position, iValue)) {
        return position + iValue.length
      }
      ~position
    }
  }

  abstract class NumberFormatter(fieldType: DateTimeFieldType,
                                 maxParsedDigits: Int,
                                 signed: Boolean)
      extends InternalPrinter()
      with InternalParser {

    protected var iFieldType: DateTimeFieldType = null
    protected var iMaxParsedDigits: Int = _
    protected var iSigned: Boolean = _

    iFieldType = fieldType
    iMaxParsedDigits = maxParsedDigits
    iSigned = signed

    def estimateParsedLength(): Int = iMaxParsedDigits

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var _position: Int = position
      var limit = Math.min(iMaxParsedDigits, text.length - _position)
      var negative = false
      var length = 0
      while (length < limit) {
        var contineFlag = true
        var c = text.charAt(_position + length)
        if (length == 0 && (c == '-' || c == '+') && iSigned) {
          negative = c == '-'
          @inline def _c = {
            c = text.charAt(_position + length + 1)
            c
          }
          if (length + 1 >= limit || _c < '0' ||
              c > '9') {
            break()
          }
          if (negative) {
            length += 1
          } else {
            _position += 1
          }
          limit = Math.min(limit + 1, text.length - _position)
          contineFlag = false
        }
        if (contineFlag) {
          if (c < '0' || c > '9') {
            break()
          }
          length += 1
        }
      }
      if (length == 0) {
        return ~_position
      }
      var value: Int = 0
      if (length >= 9) {
        _position += length
        value =
          Integer.parseInt(text.subSequence(_position, _position).toString)
      } else {
        var i = _position
        if (negative) {
          i += 1
        }
        try {
          i += 1
          value = text.charAt(i) - '0'
        } catch {
          case e: StringIndexOutOfBoundsException => return ~_position
        }
        _position += length
        while (i < _position) {
          i += 1
          value = ((value << 3) + (value << 1)) + text.charAt(i) - '0'
        }
        if (negative) {
          value = -value
        }
      }
      bucket.saveField(iFieldType, value)
      _position
    }
  }

  class UnpaddedNumber(fieldType: DateTimeFieldType,
                       maxParsedDigits: Int,
                       signed: Boolean)
      extends NumberFormatter(fieldType, maxParsedDigits, signed) {

    def estimatePrintedLength(): Int = iMaxParsedDigits

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      try {
        val field = iFieldType.getField(chrono)
        FormatUtils.appendUnpaddedInteger(appendable, field.get(instant))
      } catch {
        case e: RuntimeException => appendable.append('�')
      }
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      if (partial.isSupported(iFieldType)) {
        try {
          FormatUtils
            .appendUnpaddedInteger(appendable, partial.get(iFieldType))
        } catch {
          case e: RuntimeException => appendable.append('�')
        }
      } else {
        appendable.append('�')
      }
    }
  }

  class PaddedNumber(fieldType: DateTimeFieldType,
                     maxParsedDigits: Int,
                     signed: Boolean,
                     minPrintedDigits: Int)
      extends NumberFormatter(fieldType, maxParsedDigits, signed) {

    private var iMinPrintedDigits: Int = _
    iMinPrintedDigits = minPrintedDigits

    def estimatePrintedLength(): Int = iMaxParsedDigits

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      try {
        val field = iFieldType.getField(chrono)
        FormatUtils.appendPaddedInteger(appendable,
                                        field.get(instant),
                                        iMinPrintedDigits)
      } catch {
        case e: RuntimeException =>
          appendUnknownString(appendable, iMinPrintedDigits)
      }
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      if (partial.isSupported(iFieldType)) {
        try {
          FormatUtils.appendPaddedInteger(appendable,
                                          partial.get(iFieldType),
                                          iMinPrintedDigits)
        } catch {
          case e: RuntimeException =>
            appendUnknownString(appendable, iMinPrintedDigits)
        }
      } else {
        appendUnknownString(appendable, iMinPrintedDigits)
      }
    }
  }

  class FixedNumber(fieldType: DateTimeFieldType,
                    numDigits: Int,
                    signed: Boolean)
      extends PaddedNumber(fieldType, numDigits, signed, numDigits) {

    override def parseInto(bucket: DateTimeParserBucket,
                           text: CharSequence,
                           position: Int): Int = {
      val newPos = super.parseInto(bucket, text, position)
      if (newPos < 0) {
        return newPos
      }
      var expectedPos = position + iMaxParsedDigits
      if (newPos != expectedPos) {
        if (iSigned) {
          val c = text.charAt(position)
          if (c == '-' || c == '+') {
            expectedPos += 1
          }
        }
        if (newPos > expectedPos) {
          return ~(expectedPos + 1)
        } else if (newPos < expectedPos) {
          return ~newPos
        }
      }
      newPos
    }
  }

  class TwoDigitYear(private val `type`: DateTimeFieldType,
                     private val pivot: Int,
                     private val lenientParse: Boolean)
      extends InternalPrinter()
      with InternalParser {

    private var iLenientParse: Boolean = _
    private var iPivot: Int = _
    private var iType: DateTimeFieldType = null

    iType = `type`
    iPivot = pivot
    iLenientParse = lenientParse

    def estimateParsedLength(): Int = if (iLenientParse) 4 else 2

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var _position: Int = position
      var limit = text.length - _position
      if (!iLenientParse) {
        limit = Math.min(2, limit)
        if (limit < 2) {
          return ~_position
        }
      } else {
        var hasSignChar = false
        var negative = false
        var length = 0
        while (length < limit) {
          var continueFlag = true
          val c = text.charAt(_position + length)
          if (length == 0 && (c == '-' || c == '+')) {
            hasSignChar = true
            negative = c == '-'
            if (negative) {
              length += 1
            } else {
              _position += 1
              limit -= 1
            }
            continueFlag = false
          }
          if (continueFlag) {
            if (c < '0' || c > '9') {
              break()
            }
            length += 1
          }
        }
        if (length == 0) {
          return ~_position
        }
        if (hasSignChar || length != 2) {
          var value: Int = 0
          if (length >= 9) {
            _position += length
            value =
              Integer.parseInt(text.subSequence(_position, position).toString)
          } else {
            var i = _position
            if (negative) {
              i += 1
            }
            try {
              i += 1
              value = text.charAt(i) - '0'
            } catch {
              case e: StringIndexOutOfBoundsException => return ~_position
            }
            _position += length
            while (i < _position) {
              i += 1
              value = ((value << 3) + (value << 1)) + text.charAt(i) - '0'
            }
            if (negative) {
              value = -value
            }
          }
          bucket.saveField(iType, value)
          return _position
        }
      }
      var year: Int = 0
      var c = text.charAt(_position)
      if (c < '0' || c > '9') {
        return ~_position
      }
      year = c - '0'
      c = text.charAt(_position + 1)
      if (c < '0' || c > '9') {
        return ~_position
      }
      year = ((year << 3) + (year << 1)) + c - '0'
      var pivot = iPivot
      if (bucket.getPivotYear != null) {
        pivot = bucket.getPivotYear.intValue()
      }
      val low = pivot - 50
      var t: Int = 0
      t = if (low >= 0) low % 100 else 99 + ((low + 1) % 100)
      year += low + (if ((year < t)) 100 else 0) - t
      bucket.saveField(iType, year)
      _position + 2
    }

    def estimatePrintedLength(): Int = 2

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      val year = getTwoDigitYear(instant, chrono)
      if (year < 0) {
        appendable.append('�')
        appendable.append('�')
      } else {
        FormatUtils.appendPaddedInteger(appendable, year, 2)
      }
    }

    private def getTwoDigitYear(instant: Long, chrono: Chronology): Int = {
      try {
        var year = iType.getField(chrono).get(instant)
        if (year < 0) {
          year = -year
        }
        year % 100
      } catch {
        case e: RuntimeException => -1
      }
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      val year = getTwoDigitYear(partial)
      if (year < 0) {
        appendable.append('�')
        appendable.append('�')
      } else {
        FormatUtils.appendPaddedInteger(appendable, year, 2)
      }
    }

    private def getTwoDigitYear(partial: ReadablePartial): Int = {
      if (partial.isSupported(iType)) {
        try {
          var year = partial.get(iType)
          if (year < 0) {
            year = -year
          }
          return year % 100
        } catch {
          case e: RuntimeException =>
        }
      }
      -1
    }
  }

  object TextField {
    private val cParseCache: Map[Locale, Map[DateTimeFieldType, Array[Any]]] =
      new ConcurrentHashMap[Locale, Map[DateTimeFieldType, Array[Any]]]()
  }

  class TextField(private val iFieldType: DateTimeFieldType,
                  private val iShort: Boolean)
      extends InternalPrinter()
      with InternalParser {

    def estimatePrintedLength(): Int = if (iShort) 6 else 20

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      try {
        appendable.append(print(instant, chrono, locale))
      } catch {
        case e: RuntimeException => appendable.append('�')
      }
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      try {
        appendable.append(print(partial, locale))
      } catch {
        case e: RuntimeException => appendable.append('�')
      }
    }

    private def print(instant: Long,
                      chrono: Chronology,
                      locale: Locale): String = {
      val field = iFieldType.getField(chrono)
      if (iShort) {
        field.getAsShortText(instant, locale)
      } else {
        field.getAsText(instant, locale)
      }
    }

    private def print(partial: ReadablePartial, locale: Locale): String = {
      if (partial.isSupported(iFieldType)) {
        val field = iFieldType.getField(partial.getChronology)
        if (iShort) {
          field.getAsShortText(partial, locale)
        } else {
          field.getAsText(partial, locale)
        }
      } else {
        "�"
      }
    }

    def estimateParsedLength(): Int = estimatePrintedLength()

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      val locale = bucket.getLocale
      var validValues: Map[String, Boolean] = null
      var maxLength = 0
      var innerMap = TextField.cParseCache.get(locale)
      if (innerMap == null) {
        innerMap = new ConcurrentHashMap[DateTimeFieldType, Array[Any]]()
        TextField.cParseCache.put(locale, innerMap)
      }
      var array = innerMap.get(iFieldType)
      if (array == null) {
        validValues = new ConcurrentHashMap[String, Boolean](32)
        val dt = new MutableDateTime(0L, DateTimeZone.UTC)
        val property = dt.property(iFieldType)
        val min = property.getMinimumValueOverall
        val max = property.getMaximumValueOverall
        if (max - min > 32) {
          return ~position
        }
        maxLength = property.getMaximumTextLength(locale)
        var i = min
        while (i <= max) {
          property.set(i)
          validValues.put(property.getAsShortText(locale), true)
          validValues
            .put(property.getAsShortText(locale).toLowerCase(locale), true)
          validValues
            .put(property.getAsShortText(locale).toUpperCase(locale), true)
          validValues.put(property.getAsText(locale), true)
          validValues.put(property.getAsText(locale).toLowerCase(locale), true)
          validValues.put(property.getAsText(locale).toUpperCase(locale), true)
          i += 1
        }
        if ("en" == locale.getLanguage && iFieldType == DateTimeFieldType
              .era()) {
          validValues.put("BCE", true)
          validValues.put("bce", true)
          validValues.put("CE", true)
          validValues.put("ce", true)
          maxLength = 3
        }
        array = Array(validValues, Integer.valueOf(maxLength))
        innerMap.put(iFieldType, array)
      } else {
        validValues = array(0).asInstanceOf[Map[String, Boolean]]
        maxLength = array(1).asInstanceOf[Integer].intValue()
      }
      val limit = Math.min(text.length, position + maxLength)
      var i = limit
      while (i > position) {
        val `match` = text.subSequence(position, i).toString
        if (validValues.containsKey(`match`)) {
          bucket.saveField(iFieldType, `match`, locale)
          return i
        }
        i -= 1
      }
      ~position
    }
  }

  class Fraction(private val fieldType: DateTimeFieldType,
                 val minDigits: Int,
                 var maxDigits: Int)
      extends InternalPrinter()
      with InternalParser {

    private var iFieldType: DateTimeFieldType = null
    protected var iMinDigits: Int = _
    protected var iMaxDigits: Int = _

    iFieldType = fieldType

    if (maxDigits > 18) {
      maxDigits = 18
    }

    iMinDigits = minDigits
    iMaxDigits = maxDigits

    def estimatePrintedLength(): Int = iMaxDigits

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      printTo(appendable, instant, chrono)
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      val millis = partial.getChronology.set(partial, 0L)
      printTo(appendable, millis, partial.getChronology)
    }

    protected def printTo(appendable: Appendable,
                          instant: Long,
                          chrono: Chronology) {
      val field = iFieldType.getField(chrono)
      var minDigits = iMinDigits
      var fraction: Long = 0l
      try {
        fraction = field.remainder(instant)
      } catch {
        case e: RuntimeException => {
          appendUnknownString(appendable, minDigits)
          return
        }
      }
      if (fraction == 0) {
        while (minDigits >= 0) {
          appendable.append('0')
        }
        return
      }
      var str: String = null
      val fractionData = getFractionData(fraction, field)
      val scaled = fractionData(0)
      val maxDigits = fractionData(1).toInt
      str =
        if ((scaled & 0x7fffffff) == scaled) Integer.toString(scaled.toInt)
        else scaled.toString
      var length = str.length
      var digits = maxDigits
      while (length < digits) {
        appendable.append('0')
        minDigits -= 1
        digits -= 1
      }
      if (minDigits < digits) {
        while (minDigits < digits) {
          if (length <= 1 || str.charAt(length - 1) != '0') {
            break()
          }
          digits -= 1
          length -= 1
        }
        if (length < str.length) {
          for (i <- 0 until length) {
            appendable.append(str.charAt(i))
          }
          return
        }
      }
      appendable.append(str)
    }

    private def getFractionData(fraction: Long,
                                field: DateTimeField): Array[Long] = {
      val rangeMillis = field.getDurationField.getUnitMillis
      var scalar: Long = 0l
      var maxDigits = iMaxDigits
      while (true) {
        maxDigits match {
          case 1 =>
            scalar = 10L
            break()
          case 2 =>
            scalar = 100L
            break()
          case 3 =>
            scalar = 1000L
            break()
          case 4 =>
            scalar = 10000L
            break()
          case 5 =>
            scalar = 100000L
            break()
          case 6 =>
            scalar = 1000000L
            break()
          case 7 =>
            scalar = 10000000L
            break()
          case 8 =>
            scalar = 100000000L
            break()
          case 9 =>
            scalar = 1000000000L
            break()
          case 10 =>
            scalar = 10000000000L
            break()
          case 11 =>
            scalar = 100000000000L
            break()
          case 12 =>
            scalar = 1000000000000L
            break()
          case 13 =>
            scalar = 10000000000000L
            break()
          case 14 =>
            scalar = 100000000000000L
            break()
          case 15 =>
            scalar = 1000000000000000L
            break()
          case 16 =>
            scalar = 10000000000000000L
            break()
          case 17 =>
            scalar = 100000000000000000L
            break()
          case 18 =>
            scalar = 1000000000000000000L
            break()
          case _ =>
            scalar = 1L
            break()
        }

        if (((rangeMillis * scalar) / scalar) == rangeMillis) {
          break()
        }

        maxDigits -= 1
      }

      Array(fraction * scalar / rangeMillis, maxDigits)
    }

    def estimateParsedLength(): Int = iMaxDigits

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      val field = iFieldType.getField(bucket.getChronology)
      val limit = Math.min(iMaxDigits, text.length - position)
      var value: Long = 0
      var n = field.getDurationField.getUnitMillis * 10
      var length = 0
      while (length < limit) {
        val c = text.charAt(position + length)
        if (c < '0' || c > '9') {
          break()
        }
        length += 1
        val nn = n / 10
        value += (c - '0') * nn
        n = nn
      }
      value /= 10
      if (length == 0) {
        return ~position
      }
      if (value > Integer.MAX_VALUE) {
        return ~position
      }
      val parseField = new PreciseDateTimeField(
        DateTimeFieldType.millisOfSecond(),
        MillisDurationField.INSTANCE,
        field.getDurationField)
      bucket.saveField(parseField, value.toInt)
      position + length
    }
  }

  class TimeZoneOffset(private val zeroOffsetPrintText: String,
                       private val zeroOffsetParseText: String,
                       private val showSeparators: Boolean,
                       private var minFields: Int,
                       private var maxFields: Int)
      extends InternalPrinter()
      with InternalParser {

    private var iZeroOffsetPrintText: String = null
    private var iZeroOffsetParseText: String = null
    private var iShowSeparators: Boolean = _
    private var iMinFields: Int = _
    private var iMaxFields: Int = _

    iZeroOffsetPrintText = zeroOffsetPrintText
    iZeroOffsetParseText = zeroOffsetParseText
    iShowSeparators = showSeparators

    if (minFields <= 0 || maxFields < minFields) {
      throw new IllegalArgumentException()
    }

    if (minFields > 4) {
      minFields = 4
      maxFields = 4
    }

    iMinFields = minFields
    iMaxFields = maxFields

    def estimatePrintedLength(): Int = {
      var est = 1 + iMinFields << 1
      if (iShowSeparators) {
        est += iMinFields - 1
      }
      if (iZeroOffsetPrintText != null && iZeroOffsetPrintText.length > est) {
        est = iZeroOffsetPrintText.length
      }
      est
    }

    def printTo(buf: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      var _displayOffset: Int = displayOffset

      if (displayZone == null) {
        return
      }
      if (_displayOffset == 0 && iZeroOffsetPrintText != null) {
        buf.append(iZeroOffsetPrintText)
        return
      }
      if (_displayOffset >= 0) {
        buf.append('+')
      } else {
        buf.append('-')
        _displayOffset = -_displayOffset
      }
      val hours = _displayOffset / DateTimeConstants.MILLIS_PER_HOUR
      FormatUtils.appendPaddedInteger(buf, hours, 2)
      if (iMaxFields == 1) {
        return
      }
      _displayOffset -= hours * DateTimeConstants.MILLIS_PER_HOUR.toInt
      if (_displayOffset == 0 && iMinFields <= 1) {
        return
      }
      val minutes = _displayOffset / DateTimeConstants.MILLIS_PER_MINUTE
      if (iShowSeparators) {
        buf.append(':')
      }
      FormatUtils.appendPaddedInteger(buf, minutes, 2)
      if (iMaxFields == 2) {
        return
      }
      _displayOffset -= minutes * DateTimeConstants.MILLIS_PER_MINUTE
      if (_displayOffset == 0 && iMinFields <= 2) {
        return
      }
      val seconds = _displayOffset / DateTimeConstants.MILLIS_PER_SECOND
      if (iShowSeparators) {
        buf.append(':')
      }
      FormatUtils.appendPaddedInteger(buf, seconds, 2)
      if (iMaxFields == 3) {
        return
      }
      _displayOffset -= seconds * DateTimeConstants.MILLIS_PER_SECOND
      if (_displayOffset == 0 && iMinFields <= 3) {
        return
      }
      if (iShowSeparators) {
        buf.append('.')
      }
      FormatUtils.appendPaddedInteger(buf, _displayOffset, 3)
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale): Unit = {
      // no zone info
    }

    def estimateParsedLength(): Int = estimatePrintedLength()

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var _position = position
      var limit: Int = text.length - _position

      if (iZeroOffsetParseText != null) {
        if (iZeroOffsetParseText.length == 0) {
          if (limit > 0) {
            val c: Char = text.charAt(_position)
            if (c == '-' || c == '+') {
              break() //todo: label break is not supported
            }
          }
          bucket.setOffset(Integer.valueOf(0))
          return _position
        }
        if (csStartsWithIgnoreCase(text, _position, iZeroOffsetParseText)) {
          bucket.setOffset(Integer.valueOf(0))
          return _position + iZeroOffsetParseText.length
        }
      } //todo: labels is not supported
      // Format to expect is sign character followed by at least one digit.

      if (limit <= 1) {
        return ~_position
      }

      var negative: Boolean = false
      var c: Char = text.charAt(_position)
      if (c == '-') {
        negative = true
      } else if (c == '+') {
        negative = false
      } else {
        return ~_position
      }

      limit -= 1
      _position += 1

      // Format following sign is one of:
      //
      // hh
      // hhmm
      // hhmmss
      // hhmmssSSS
      // hh:mm
      // hh:mm:ss
      // hh:mm:ss.SSS
      // First parse hours.

      if (digitCount(text, _position, 2) < 2) {
        return ~_position
      }

      var offset: Int = 0

      val hours: Int = FormatUtils.parseTwoDigits(text, _position)
      if (hours > 23) {
        return ~_position
      }
      offset = hours * DateTimeConstants.MILLIS_PER_HOUR
      limit -= 2
      _position += 2

      {
        if (limit <= 0) {
          break() //todo: label break is not supported
        }
        var expectSeparators: Boolean = false
        c = text.charAt(_position)
        if (c == ':') {
          expectSeparators = true
          limit -= 1
          _position += 1
        } else if (c >= '0' && c <= '9') {
          expectSeparators = false
        } else {
          break() //todo: label break is not supported
        }
        var count: Int = digitCount(text, _position, 2)
        if (count == 0 && !expectSeparators) {
          break() //todo: label break is not supported
        } else if (count < 2) {
          return ~_position
        }
        val minutes: Int = FormatUtils.parseTwoDigits(text, _position)
        if (minutes > 59) {
          return ~_position
        }
        offset += minutes * DateTimeConstants.MILLIS_PER_MINUTE
        limit -= 2
        _position += 2
        if (limit <= 0) {
          break() //todo: label break is not supported
        }
        if (expectSeparators) {
          if (text.charAt(_position) != ':') {
            break() //todo: label break is not supported
          }
          limit -= 1
          _position += 1
        }
        count = digitCount(text, _position, 2)
        if (count == 0 && !expectSeparators) {
          break() //todo: label break is not supported
        } else if (count < 2) {
          return ~_position
        }
        val seconds: Int = FormatUtils.parseTwoDigits(text, _position)
        if (seconds > 59) {
          return ~_position
        }
        offset += seconds * DateTimeConstants.MILLIS_PER_SECOND
        limit -= 2
        _position += 2
        if (limit <= 0) {
          break() //todo: label break is not supported
        }
        if (expectSeparators) {
          if (text.charAt(_position) != '.' && text.charAt(_position) != ',') {
            break() //todo: label break is not supported
          }
          limit -= 1
          _position += 1
        }
        count = digitCount(text, _position, 3)
        if (count == 0 && !expectSeparators) {
          break() //todo: label break is not supported
        } else if (count < 1) {
          return ~_position
        }
        offset += (text.charAt({
          _position += 1
          _position - 1
        }) - '0') * 100
        if (count > 1) {
          offset += (text.charAt({
            _position += 1
            _position - 1
          }) - '0') * 10
          if (count > 2) {
            offset += text.charAt({
              _position += 1
              _position - 1
            }) - '0'
          }
        }
      } //todo: labels is not supported

      bucket.setOffset(Integer.valueOf(if (negative) -offset else offset))
      _position
    }

    private def digitCount(text: CharSequence,
                           position: Int,
                           amount: Int): Int = {
      var _amount: Int = amount
      var limit = Math.min(text.length - position, _amount)
      _amount = 0
      while (limit > 0) {
        val c = text.charAt(position + _amount)
        if (c < '0' || c > '9') {
          break()
        }
        _amount += 1
        limit -= 1
      }
      _amount
    }
  }

  object TimeZoneName {
    val LONG_NAME = 0
    val SHORT_NAME = 1
  }

  class TimeZoneName(private val iType: Int,
                     private val iParseLookup: Map[String, DateTimeZone])
      extends InternalPrinter()
      with InternalParser {

    def estimatePrintedLength(): Int =
      if (iType == TimeZoneName.SHORT_NAME) 4 else 20

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      appendable.append(print(instant - displayOffset, displayZone, locale))
    }

    private def print(instant: Long,
                      displayZone: DateTimeZone,
                      locale: Locale): String = {
      if (displayZone == null) {
        return ""
      }
      iType match {
        case TimeZoneName.LONG_NAME =>
          return displayZone.getName(instant, locale)
        case TimeZoneName.SHORT_NAME =>
          return displayZone.getShortName(instant, locale)
      }
      ""
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {}

    def estimateParsedLength(): Int =
      if (iType == TimeZoneName.SHORT_NAME) 4 else 20

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var parseLookup = iParseLookup
      parseLookup =
        if (parseLookup != null) parseLookup
        else DateTimeUtils.getDefaultTimeZoneNames
      var matched: String = null

      val iterator = parseLookup.keySet().iterator()

      while (iterator.hasNext) {
        val name = iterator.next()
        if (csStartsWith(text, position, name)) {
          if (matched == null || name.length > matched.length) {
            matched = name
          }
        }
      }

      if (matched != null) {
        bucket.setZone(parseLookup.get(matched))
        return position + matched.length
      }
      ~position
    }
  }

  class TimeZoneId
      extends Enumeration
      with InternalPrinter
      with InternalParser {
    import TimeZoneId._
    def estimatePrintedLength(): Int = MAX_LENGTH

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      appendable.append(if (displayZone != null) displayZone.getID else "")
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {}

    def estimateParsedLength(): Int = MAX_LENGTH

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var best: String = null

      val iterator = ALL_IDS.iterator()

      while (iterator.hasNext) {
        val id = iterator.next()
        if (csStartsWith(text, position, id)) {
          if (best == null || id.length > best.length) {
            best = id
          }
        }
      }

      if (best != null) {
        bucket.setZone(DateTimeZone.forID(best))
        return position + best.length
      }
      ~position
    }
  }

  object TimeZoneId extends Enumeration {

    val ALL_IDS = DateTimeZone.getAvailableIDs

    val INSTANCE = new TimeZoneId()

    var max = 0
    val MAX_LENGTH = max

    val iterator = ALL_IDS.iterator()

    while (iterator.hasNext) {
      val id = iterator.next()
      max = Math.max(max, id.length)
    }

  }

  class Composite(elementPairs: List[Any])
      extends InternalPrinter()
      with InternalParser {

    private var iPrinters: Array[InternalPrinter] = null
    private var iParsers: Array[InternalParser] = null
    private var iPrintedLengthEstimate: Int = _
    private var iParsedLengthEstimate: Int = _

    val printerList = new ArrayList[Any]()

    val parserList = new ArrayList[Any]()

    decompose(elementPairs, printerList, parserList)

    if (printerList.contains(null) || printerList.isEmpty) {
      iPrinters = null
      iPrintedLengthEstimate = 0
    } else {
      val size = printerList.size
      iPrinters = Array.ofDim[InternalPrinter](size)
      var printEst = 0
      for (i <- 0 until size) {
        val printer = printerList.get(i).asInstanceOf[InternalPrinter]
        printEst += printer.estimatePrintedLength()
        iPrinters(i) = printer
      }
      iPrintedLengthEstimate = printEst
    }

    if (parserList.contains(null) || parserList.isEmpty) {
      iParsers = null
      iParsedLengthEstimate = 0
    } else {
      val size = parserList.size
      iParsers = Array.ofDim[InternalParser](size)
      var parseEst = 0
      for (i <- 0 until size) {
        val parser = parserList.get(i).asInstanceOf[InternalParser]
        parseEst += parser.estimateParsedLength()
        iParsers(i) = parser
      }
      iParsedLengthEstimate = parseEst
    }

    def estimatePrintedLength(): Int = iPrintedLengthEstimate

    def printTo(appendable: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      var _locale: Locale = locale
      val elements = iPrinters
      if (elements == null) {
        throw new UnsupportedOperationException()
      }
      if (_locale == null) {
        _locale = Locale.getDefault
      }
      val len = elements.length
      for (i <- 0 until len) {
        elements(i).printTo(appendable,
                            instant,
                            chrono,
                            displayOffset,
                            displayZone,
                            _locale)
      }
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      var _locale: Locale = locale
      val elements = iPrinters
      if (elements == null) {
        throw new UnsupportedOperationException()
      }
      if (_locale == null) {
        _locale = Locale.getDefault
      }
      val len = elements.length
      for (i <- 0 until len) {
        elements(i).printTo(appendable, partial, _locale)
      }
    }

    def estimateParsedLength(): Int = iParsedLengthEstimate

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      var _position: Int = position
      val elements = iParsers
      if (elements == null) {
        throw new UnsupportedOperationException()
      }
      val len = elements.length
      var i = 0
      while (i < len && _position >= 0) {
        _position = elements(i).parseInto(bucket, text, _position)
        i += 1
      }
      _position
    }

    def isPrinter(): Boolean = iPrinters != null

    def isParser(): Boolean = iParsers != null

    private def decompose(elementPairs: List[Any],
                          printerList: List[Any],
                          parserList: List[Any]) {
      val size = elementPairs.size
      var i = 0
      while (i < size) {
        var element = elementPairs.get(i)
        if (element.isInstanceOf[Composite]) {
          addArrayToList(
            printerList,
            element.asInstanceOf[Composite].iPrinters.map(_.asInstanceOf[Any]))
        } else {
          printerList.add(element)
        }
        element = elementPairs.get(i + 1)
        if (element.isInstanceOf[Composite]) {
          addArrayToList(
            parserList,
            element.asInstanceOf[Composite].iParsers.map(_.asInstanceOf[Any]))
        } else {
          parserList.add(element)
        }
        i += 2
      }
    }

    private def addArrayToList(list: List[Any], array: Array[Any]) {
      if (array != null) {
        for (i <- 0 until array.length) {
          list.add(array(i))
        }
      }
    }
  }

  class MatchingParser(private val parsers: Array[InternalParser])
      extends InternalParser() {

    private var iParsedLengthEstimate: Int = _
    private var iParsers: Array[InternalParser] = null

    iParsers = parsers
    var est = 0

    var i = parsers.length
    while (i >= 0) {
      val parser = parsers(i)
      if (parser != null) {
        val len = parser.estimateParsedLength()
        if (len > est) {
          est = len
        }
      }
    }

    iParsedLengthEstimate = est

    def estimateParsedLength(): Int = iParsedLengthEstimate

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      val parsers = iParsers
      val length = parsers.length
      val originalState = bucket.saveState()
      var isOptional = false
      var bestValidPos = position
      var bestValidState: AnyRef = null
      var bestInvalidPos = position
      for (i <- 0 until length) {
        val parser = parsers(i)
        if (parser == null) {
          if (bestValidPos <= position) {
            return position
          }
          isOptional = true
          break()
        }
        var parsePos = parser.parseInto(bucket, text, position)
        if (parsePos >= position) {
          if (parsePos > bestValidPos) {
            if (parsePos >= text.length || (i + 1) >= length || parsers(i + 1) == null) {
              return parsePos
            }
            bestValidPos = parsePos
            bestValidState = bucket.saveState()
          }
        } else {
          if (parsePos < 0) {
            parsePos = ~parsePos
            if (parsePos > bestInvalidPos) {
              bestInvalidPos = parsePos
            }
          }
        }
        bucket.restoreState(originalState)
      }
      if (bestValidPos > position || (bestValidPos == position && isOptional)) {
        if (bestValidState != null) {
          bucket.restoreState(bestValidState)
        }
        return bestValidPos
      }
      ~bestInvalidPos
    }
  }

  def csStartsWith(text: CharSequence,
                   position: Int,
                   search: String): Boolean = {
    val searchLen = search.length
    if ((text.length - position) < searchLen) {
      return false
    }
    for (i <- 0 until searchLen
         if text.charAt(position + i) != search.charAt(i)) {
      return false
    }
    true
  }

  def csStartsWithIgnoreCase(text: CharSequence,
                             position: Int,
                             search: String): Boolean = {
    val searchLen = search.length
    if ((text.length - position) < searchLen) {
      return false
    }
    for (i <- 0 until searchLen) {
      val ch1 = text.charAt(position + i)
      val ch2 = search.charAt(i)
      if (ch1 != ch2) {
        val u1 = Character.toUpperCase(ch1)
        val u2 = Character.toUpperCase(ch2)
        if (u1 != u2 &&
            Character.toLowerCase(u1) != Character.toLowerCase(u2)) {
          return false
        }
      }
    }
    true
  }
}

class DateTimeFormatterBuilder {

  private val iElementPairs: ArrayList[Any] = new ArrayList[Any]()
  private var iFormatter: Any = null

  def toFormatter(): DateTimeFormatter = {
    val f = getFormatter
    var printer: InternalPrinter = null
    if (isPrinter(f)) {
      printer = f.asInstanceOf[InternalPrinter]
    }
    var parser: InternalParser = null
    if (isParser(f)) {
      parser = f.asInstanceOf[InternalParser]
    }
    if (printer != null || parser != null) {
      return new DateTimeFormatter(printer, parser)
    }
    throw new UnsupportedOperationException(
      "Both printing and parsing not supported")
  }

  def toPrinter(): DateTimePrinter = {
    val f = getFormatter
    if (isPrinter(f)) {
      val ip = f.asInstanceOf[InternalPrinter]
      return InternalPrinterDateTimePrinter.of(ip)
    }
    throw new UnsupportedOperationException("Printing is not supported")
  }

  def toParser(): DateTimeParser = {
    val f = getFormatter
    if (isParser(f)) {
      val ip = f.asInstanceOf[InternalParser]
      return InternalParserDateTimeParser.of(ip)
    }
    throw new UnsupportedOperationException("Parsing is not supported")
  }

  def canBuildFormatter(): Boolean = isFormatter(getFormatter)

  def canBuildPrinter(): Boolean = isPrinter(getFormatter)

  def canBuildParser(): Boolean = isParser(getFormatter)

  def clear() {
    iFormatter = null
    iElementPairs.clear()
  }

  def append(formatter: DateTimeFormatter): DateTimeFormatterBuilder = {
    if (formatter == null) {
      throw new IllegalArgumentException("No formatter supplied")
    }
    append0(formatter.getPrinter0, formatter.getParser0)
  }

  def append(printer: DateTimePrinter): DateTimeFormatterBuilder = {
    checkPrinter(printer)
    append0(DateTimePrinterInternalPrinter.of(printer), null)
  }

  def append(parser: DateTimeParser): DateTimeFormatterBuilder = {
    checkParser(parser)
    append0(null, DateTimeParserInternalParser.of(parser))
  }

  def append(printer: DateTimePrinter,
             parser: DateTimeParser): DateTimeFormatterBuilder = {
    checkPrinter(printer)
    checkParser(parser)
    append0(DateTimePrinterInternalPrinter.of(printer),
            DateTimeParserInternalParser.of(parser))
  }

  def append(printer: DateTimePrinter,
             parsers: Array[DateTimeParser]): DateTimeFormatterBuilder = {
    if (printer != null) {
      checkPrinter(printer)
    }
    if (parsers == null) {
      throw new IllegalArgumentException("No parsers supplied")
    }
    val length = parsers.length
    if (length == 1) {
      if (parsers(0) == null) {
        throw new IllegalArgumentException("No parser supplied")
      }
      return append0(DateTimePrinterInternalPrinter.of(printer),
                     DateTimeParserInternalParser.of(parsers(0)))
    }
    val copyOfParsers = Array.ofDim[InternalParser](length)
    var i: Int = 0
    i = 0
    while (i < length - 1) {
      if ({
        copyOfParsers(i) = DateTimeParserInternalParser.of(parsers(i));
        copyOfParsers(i)
      } ==
        null) {
        throw new IllegalArgumentException("Incomplete parser array")
      }
      i += 1
    }
    copyOfParsers(i) = DateTimeParserInternalParser.of(parsers(i))
    append0(DateTimePrinterInternalPrinter.of(printer),
            new MatchingParser(copyOfParsers))
  }

  def appendOptional(parser: DateTimeParser): DateTimeFormatterBuilder = {
    checkParser(parser)
    val parsers = Array(DateTimeParserInternalParser.of(parser), null)
    append0(null, new MatchingParser(parsers))
  }

  private def checkParser(parser: DateTimeParser) {
    if (parser == null) {
      throw new IllegalArgumentException("No parser supplied")
    }
  }

  private def checkPrinter(printer: DateTimePrinter) {
    if (printer == null) {
      throw new IllegalArgumentException("No printer supplied")
    }
  }

  private def append0(element: AnyRef): DateTimeFormatterBuilder = {
    iFormatter = null
    iElementPairs.add(element)
    iElementPairs.add(element)
    this
  }

  private def append0(printer: InternalPrinter,
                      parser: InternalParser): DateTimeFormatterBuilder = {
    iFormatter = null
    iElementPairs.add(printer)
    iElementPairs.add(parser)
    this
  }

  def appendLiteral(c: Char): DateTimeFormatterBuilder =
    append0(new CharacterLiteral(c))

  def appendLiteral(text: String): DateTimeFormatterBuilder = {
    if (text == null) {
      throw new IllegalArgumentException("Literal must not be null")
    }
    text.length match {
      case 0 => this
      case 1 => append0(new CharacterLiteral(text.charAt(0)))
      case _ => append0(new StringLiteral(text))
    }
  }

  def appendDecimal(fieldType: DateTimeFieldType,
                    minDigits: Int,
                    maxDigits: Int): DateTimeFormatterBuilder = {
    var _maxDigits: Int = maxDigits
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    if (_maxDigits < minDigits) {
      _maxDigits = minDigits
    }
    if (minDigits < 0 || _maxDigits <= 0) {
      throw new IllegalArgumentException()
    }
    if (minDigits <= 1) {
      append0(new UnpaddedNumber(fieldType, _maxDigits, false))
    } else {
      append0(new PaddedNumber(fieldType, _maxDigits, false, minDigits))
    }
  }

  def appendFixedDecimal(fieldType: DateTimeFieldType,
                         numDigits: Int): DateTimeFormatterBuilder = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    if (numDigits <= 0) {
      throw new IllegalArgumentException(
        "Illegal number of digits: " + numDigits)
    }
    append0(new FixedNumber(fieldType, numDigits, false))
  }

  def appendSignedDecimal(fieldType: DateTimeFieldType,
                          minDigits: Int,
                          maxDigits: Int): DateTimeFormatterBuilder = {
    var _maxDigits: Int = maxDigits
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    if (_maxDigits < minDigits) {
      _maxDigits = minDigits
    }
    if (minDigits < 0 || _maxDigits <= 0) {
      throw new IllegalArgumentException()
    }
    if (minDigits <= 1) {
      append0(new UnpaddedNumber(fieldType, _maxDigits, true))
    } else {
      append0(new PaddedNumber(fieldType, _maxDigits, true, minDigits))
    }
  }

  def appendFixedSignedDecimal(fieldType: DateTimeFieldType,
                               numDigits: Int): DateTimeFormatterBuilder = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    if (numDigits <= 0) {
      throw new IllegalArgumentException(
        "Illegal number of digits: " + numDigits)
    }
    append0(new FixedNumber(fieldType, numDigits, true))
  }

  def appendText(fieldType: DateTimeFieldType): DateTimeFormatterBuilder = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    append0(new TextField(fieldType, false))
  }

  def appendShortText(fieldType: DateTimeFieldType): DateTimeFormatterBuilder = {
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    append0(new TextField(fieldType, true))
  }

  def appendFraction(fieldType: DateTimeFieldType,
                     minDigits: Int,
                     maxDigits: Int): DateTimeFormatterBuilder = {
    var _maxDigits: Int = maxDigits
    if (fieldType == null) {
      throw new IllegalArgumentException("Field type must not be null")
    }
    if (_maxDigits < minDigits) {
      _maxDigits = minDigits
    }
    if (minDigits < 0 || _maxDigits <= 0) {
      throw new IllegalArgumentException()
    }
    append0(new Fraction(fieldType, minDigits, _maxDigits))
  }

  def appendFractionOfSecond(minDigits: Int,
                             maxDigits: Int): DateTimeFormatterBuilder = {
    appendFraction(DateTimeFieldType.secondOfDay(), minDigits, maxDigits)
  }

  def appendFractionOfMinute(minDigits: Int,
                             maxDigits: Int): DateTimeFormatterBuilder = {
    appendFraction(DateTimeFieldType.minuteOfDay(), minDigits, maxDigits)
  }

  def appendFractionOfHour(minDigits: Int,
                           maxDigits: Int): DateTimeFormatterBuilder = {
    appendFraction(DateTimeFieldType.hourOfDay(), minDigits, maxDigits)
  }

  def appendFractionOfDay(minDigits: Int,
                          maxDigits: Int): DateTimeFormatterBuilder = {
    appendFraction(DateTimeFieldType.dayOfYear(), minDigits, maxDigits)
  }

  def appendMillisOfSecond(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.millisOfSecond(), minDigits, 3)
  }

  def appendMillisOfDay(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.millisOfDay(), minDigits, 8)
  }

  def appendSecondOfMinute(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.secondOfMinute(), minDigits, 2)
  }

  def appendSecondOfDay(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.secondOfDay(), minDigits, 5)
  }

  def appendMinuteOfHour(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.minuteOfHour(), minDigits, 2)
  }

  def appendMinuteOfDay(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.minuteOfDay(), minDigits, 4)
  }

  def appendHourOfDay(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.hourOfDay(), minDigits, 2)
  }

  def appendClockhourOfDay(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.clockhourOfDay(), minDigits, 2)
  }

  def appendHourOfHalfday(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.hourOfHalfday(), minDigits, 2)
  }

  def appendClockhourOfHalfday(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.clockhourOfHalfday(), minDigits, 2)
  }

  def appendDayOfWeek(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.dayOfWeek(), minDigits, 1)
  }

  def appendDayOfMonth(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.dayOfMonth(), minDigits, 2)
  }

  def appendDayOfYear(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.dayOfYear(), minDigits, 3)
  }

  def appendWeekOfWeekyear(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.weekOfWeekyear(), minDigits, 2)
  }

  def appendWeekyear(minDigits: Int,
                     maxDigits: Int): DateTimeFormatterBuilder = {
    appendSignedDecimal(DateTimeFieldType.weekyear(), minDigits, maxDigits)
  }

  def appendMonthOfYear(minDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.monthOfYear(), minDigits, 2)
  }

  def appendYear(minDigits: Int, maxDigits: Int): DateTimeFormatterBuilder = {
    appendSignedDecimal(DateTimeFieldType.year(), minDigits, maxDigits)
  }

  def appendTwoDigitYear(pivot: Int): DateTimeFormatterBuilder =
    appendTwoDigitYear(pivot, lenientParse = false)

  def appendTwoDigitYear(pivot: Int,
                         lenientParse: Boolean): DateTimeFormatterBuilder = {
    append0(new TwoDigitYear(DateTimeFieldType.year(), pivot, lenientParse))
  }

  def appendTwoDigitWeekyear(pivot: Int): DateTimeFormatterBuilder =
    appendTwoDigitWeekyear(pivot, lenientParse = false)

  def appendTwoDigitWeekyear(
      pivot: Int,
      lenientParse: Boolean): DateTimeFormatterBuilder = {
    append0(
      new TwoDigitYear(DateTimeFieldType.weekyear(), pivot, lenientParse))
  }

  def appendYearOfEra(minDigits: Int,
                      maxDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.yearOfEra(), minDigits, maxDigits)
  }

  def appendYearOfCentury(minDigits: Int,
                          maxDigits: Int): DateTimeFormatterBuilder = {
    appendDecimal(DateTimeFieldType.yearOfCentury(), minDigits, maxDigits)
  }

  def appendCenturyOfEra(minDigits: Int,
                         maxDigits: Int): DateTimeFormatterBuilder = {
    appendSignedDecimal(DateTimeFieldType.centuryOfEra(), minDigits, maxDigits)
  }

  def appendHalfdayOfDayText(): DateTimeFormatterBuilder = {
    appendText(DateTimeFieldType.halfdayOfDay())
  }

  def appendDayOfWeekText(): DateTimeFormatterBuilder = {
    appendText(DateTimeFieldType.dayOfWeek())
  }

  def appendDayOfWeekShortText(): DateTimeFormatterBuilder = {
    appendShortText(DateTimeFieldType.dayOfWeek())
  }

  def appendMonthOfYearText(): DateTimeFormatterBuilder = {
    appendText(DateTimeFieldType.monthOfYear())
  }

  def appendMonthOfYearShortText(): DateTimeFormatterBuilder = {
    appendShortText(DateTimeFieldType.monthOfYear())
  }

  def appendEraText(): DateTimeFormatterBuilder =
    appendText(DateTimeFieldType.era())

  def appendTimeZoneName(): DateTimeFormatterBuilder = {
    append0(new TimeZoneName(TimeZoneName.LONG_NAME, null), null)
  }

  def appendTimeZoneName(
      parseLookup: Map[String, DateTimeZone]): DateTimeFormatterBuilder = {
    val pp = new TimeZoneName(TimeZoneName.LONG_NAME, parseLookup)
    append0(pp, pp)
  }

  def appendTimeZoneShortName(): DateTimeFormatterBuilder = {
    append0(new TimeZoneName(TimeZoneName.SHORT_NAME, null), null)
  }

  def appendTimeZoneShortName(
      parseLookup: Map[String, DateTimeZone]): DateTimeFormatterBuilder = {
    val pp = new TimeZoneName(TimeZoneName.SHORT_NAME, parseLookup)
    append0(pp, pp)
  }

  def appendTimeZoneId(): DateTimeFormatterBuilder = {
    append0(TimeZoneId.INSTANCE, TimeZoneId.INSTANCE)
  }

  def appendTimeZoneOffset(zeroOffsetText: String,
                           showSeparators: Boolean,
                           minFields: Int,
                           maxFields: Int): DateTimeFormatterBuilder = {
    append0(
      new TimeZoneOffset(zeroOffsetText,
                         zeroOffsetText,
                         showSeparators,
                         minFields,
                         maxFields))
  }

  def appendTimeZoneOffset(zeroOffsetPrintText: String,
                           zeroOffsetParseText: String,
                           showSeparators: Boolean,
                           minFields: Int,
                           maxFields: Int): DateTimeFormatterBuilder = {
    append0(
      new TimeZoneOffset(zeroOffsetPrintText,
                         zeroOffsetParseText,
                         showSeparators,
                         minFields,
                         maxFields))
  }

  def appendPattern(pattern: String): DateTimeFormatterBuilder = {
    DateTimeFormat.appendPatternTo(this, pattern)
    this
  }

  private def getFormatter(): Any = {
    var f: Any = iFormatter
    if (f == null) {
      if (iElementPairs.size == 2) {
        val printer: Any = iElementPairs.get(0)
        val parser: Any = iElementPairs.get(1)
        if (printer != null) {
          if (printer == parser || parser == null) {
            f = printer
          }
        } else {
          f = parser
        }
      }
      if (f == null) {
        f = new Composite(iElementPairs)
      }
      iFormatter = f
    }
    f
  }

  private def isPrinter(f: Any): Boolean = {
    if (f.isInstanceOf[InternalPrinter]) {
      if (f.isInstanceOf[Composite]) {
        return f.asInstanceOf[Composite].isPrinter
      }
      return true
    }
    false
  }

  private def isParser(f: Any): Boolean = {
    if (f.isInstanceOf[InternalParser]) {
      if (f.isInstanceOf[Composite]) {
        return f.asInstanceOf[Composite].isParser
      }
      return true
    }
    false
  }

  private def isFormatter(f: Any): Boolean = isPrinter(f) || isParser(f)
}
