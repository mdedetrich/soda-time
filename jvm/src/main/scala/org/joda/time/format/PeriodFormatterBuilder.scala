package org.joda.time.format

import java.io.Writer
import java.util.ArrayList
import java.util.Arrays
import java.util.Collections
import java.util.Comparator
import java.util.HashSet
import java.util.List
import java.util.Locale
import java.util.Set
import java.util.TreeSet
import java.util.concurrent.ConcurrentHashMap
import java.util.function.Consumer
import java.util.regex.Pattern
import org.joda.time.DateTimeConstants
import org.joda.time.DurationFieldType
import org.joda.time.PeriodType
import org.joda.time.ReadWritablePeriod
import org.joda.time.ReadablePeriod
import PeriodFormatterBuilder._
import scala.util.control.Breaks._

object PeriodFormatterBuilder {

  private val PRINT_ZERO_RARELY_FIRST = 1
  private val PRINT_ZERO_RARELY_LAST = 2
  private val PRINT_ZERO_IF_SUPPORTED = 3
  private val PRINT_ZERO_ALWAYS = 4
  private val PRINT_ZERO_NEVER = 5
  private val YEARS = 0
  private val MONTHS = 1
  private val WEEKS = 2
  private val DAYS = 3
  private val HOURS = 4
  private val MINUTES = 5
  private val SECONDS = 6
  private val MILLIS = 7
  private val SECONDS_MILLIS = 8
  private val SECONDS_OPTIONAL_MILLIS = 9
  private val MAX_FIELD = SECONDS_OPTIONAL_MILLIS
  private val PATTERNS = new ConcurrentHashMap[String, Pattern]()

  private def toFormatter(elementPairs: List[Any], notPrinter: Boolean, notParser: Boolean): PeriodFormatter = {
    if (notPrinter && notParser) {
      throw new IllegalStateException("Builder has created neither a printer nor a parser")
    }
    val size = elementPairs.size
    if (size >= 2 && elementPairs.get(0).isInstanceOf[Separator]) {
      var sep = elementPairs.get(0).asInstanceOf[Separator]
      if (sep.iAfterParser == null && sep.iAfterPrinter == null) {
        val f = toFormatter(elementPairs.subList(2, size), notPrinter, notParser)
        sep = sep.finish(f.getPrinter, f.getParser)
        return new PeriodFormatter(sep, sep)
      }
    }
    val comp = createComposite(elementPairs)
    if (notPrinter) {
      new PeriodFormatter(null, comp(1).asInstanceOf[PeriodParser])
    } else if (notParser) {
      new PeriodFormatter(comp(0).asInstanceOf[PeriodPrinter], null)
    } else {
      new PeriodFormatter(comp(0).asInstanceOf[PeriodPrinter], comp(1).asInstanceOf[PeriodParser])
    }
  }

  private def createComposite(elementPairs: List[Any]): Array[Any] = elementPairs.size match {
    case 0 => Array(Literal.EMPTY, Literal.EMPTY)
    case 1 => Array(elementPairs.get(0), elementPairs.get(1))
    case _ =>
      val comp = new Composite(elementPairs)
      Array(comp, comp)

  }

  trait PeriodFieldAffix {

    def calculatePrintedLength(value: Int): Int

    def printTo(buf: StringBuffer, value: Int): Unit

    def printTo(out: Writer, value: Int): Unit

    def parse(periodStr: String, position: Int): Int

    def scan(periodStr: String, position: Int): Int

    def getAffixes(): Array[String]

    def finish(affixesToIgnore: Set[PeriodFieldAffix]): Unit
  }

  abstract class IgnorableAffix extends PeriodFieldAffix {

    @volatile private var iOtherAffixes: Array[String] = _

    def finish(periodFieldAffixesToIgnore: Set[PeriodFieldAffix]) {
      if (iOtherAffixes == null) {
        var shortestAffixLength = Integer.MAX_VALUE
        var shortestAffix: String = null
        for (affix <- getAffixes if affix.length < shortestAffixLength) {
          shortestAffixLength = affix.length
          shortestAffix = affix
        }
        val affixesToIgnore = new HashSet[String]()
        periodFieldAffixesToIgnore.forEach(new Consumer[PeriodFieldAffix] {
          def accept(periodFieldAffixToIgnore: PeriodFieldAffix): Unit = {
            if (periodFieldAffixToIgnore != null) {
              periodFieldAffixToIgnore.getAffixes().foreach{affixToIgnore =>
                if (affixToIgnore.length > shortestAffixLength || (affixToIgnore.equalsIgnoreCase(shortestAffix) && !(affixToIgnore == shortestAffix))) {
                  affixesToIgnore.add(affixToIgnore)
                }
              }
            }
          }
        })
        iOtherAffixes = affixesToIgnore.toArray(Array.ofDim[String](affixesToIgnore.size))
      }
    }

    protected def matchesOtherAffix(textLength: Int, periodStr: String, position: Int): Boolean = {
      if (iOtherAffixes != null) {
        for (affixToIgnore <- iOtherAffixes) {
          val textToIgnoreLength = affixToIgnore.length
          if ((textLength < textToIgnoreLength &&
            periodStr.regionMatches(true, position, affixToIgnore, 0, textToIgnoreLength)) ||
            (textLength == textToIgnoreLength &&
              periodStr.regionMatches(false, position, affixToIgnore, 0, textToIgnoreLength))) {
            return true
          }
        }
      }
      false
    }
  }

  class SimpleAffix(private val iText: String) extends IgnorableAffix {

    def calculatePrintedLength(value: Int): Int = iText.length

    def printTo(buf: StringBuffer, value: Int) {
      buf.append(iText)
    }

    def printTo(out: Writer, value: Int) {
      out.write(iText)
    }

    def parse(periodStr: String, position: Int): Int = {
      val text = iText
      val textLength = text.length
      if (periodStr.regionMatches(true, position, text, 0, textLength)) {
        if (!matchesOtherAffix(textLength, periodStr, position)) {
          return position + textLength
        }
      }
      ~position
    }

    def scan(periodStr: String, position: Int): Int = {
      val text = iText
      val textLength = text.length
      val sourceLength = periodStr.length

      var flag = true

      while(flag) {
        for (pos <- position until sourceLength) {
          if (periodStr.regionMatches(true, pos, text, 0, textLength)) {
            if (!matchesOtherAffix(textLength, periodStr, pos)) {
              return pos
            }
          }
          periodStr.charAt(pos) match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | ',' | '+' | '-' =>
              flag = false
              break()
            case _ => break()
          }
        }
      }
      ~position
    }

    def getAffixes(): Array[String] = Array(iText)
  }

  class PluralAffix(private val iSingularText: String, private val iPluralText: String)
    extends IgnorableAffix {

    def calculatePrintedLength(value: Int): Int = {
      (if (value == 1) iSingularText else iPluralText).length
    }

    def printTo(buf: StringBuffer, value: Int) {
      buf.append(if (value == 1) iSingularText else iPluralText)
    }

    def printTo(out: Writer, value: Int) {
      out.write(if (value == 1) iSingularText else iPluralText)
    }

    def parse(periodStr: String, position: Int): Int = {
      var text1 = iPluralText
      var text2 = iSingularText
      if (text1.length < text2.length) {
        val temp = text1
        text1 = text2
        text2 = temp
      }
      if (periodStr.regionMatches(true, position, text1, 0, text1.length)) {
        if (!matchesOtherAffix(text1.length, periodStr, position)) {
          return position + text1.length
        }
      }
      if (periodStr.regionMatches(true, position, text2, 0, text2.length)) {
        if (!matchesOtherAffix(text2.length, periodStr, position)) {
          return position + text2.length
        }
      }
      ~position
    }

    def scan(periodStr: String, position: Int): Int = {
      var text1 = iPluralText
      var text2 = iSingularText
      if (text1.length < text2.length) {
        val temp = text1
        text1 = text2
        text2 = temp
      }
      val textLength1 = text1.length
      val textLength2 = text2.length
      val sourceLength = periodStr.length
      for (pos <- position until sourceLength) {
        if (periodStr.regionMatches(true, pos, text1, 0, textLength1)) {
          if (!matchesOtherAffix(text1.length, periodStr, pos)) {
            return pos
          }
        }
        if (periodStr.regionMatches(true, pos, text2, 0, textLength2)) {
          if (!matchesOtherAffix(text2.length, periodStr, pos)) {
            return pos
          }
        }
      }
      ~position
    }

    def getAffixes(): Array[String] = Array(iSingularText, iPluralText)
  }

  object RegExAffix {

    private val LENGTH_DESC_COMPARATOR = new Comparator[String]() {

      def compare(o1: String, o2: String): Int = o2.length - o1.length
    }
  }

  class RegExAffix(regExes: Array[String], texts: Array[String]) extends IgnorableAffix {

    private val iSuffixes = texts.clone()

    private val iPatterns = new Array[Pattern](regExes.length)

    private val iSuffixesSortedDescByLength = iSuffixes.clone()

    for (i <- 0 until regExes.length) {
      var pattern = PATTERNS.get(regExes(i))
      if (pattern == null) {
        pattern = Pattern.compile(regExes(i))
        PATTERNS.putIfAbsent(regExes(i), pattern)
      }
      iPatterns(i) = pattern
    }

    Arrays.sort(iSuffixesSortedDescByLength, RegExAffix.LENGTH_DESC_COMPARATOR)

    private def selectSuffixIndex(value: Int): Int = {
      val valueString = String.valueOf(value)
      for (i <- 0 until iPatterns.length if iPatterns(i).matcher(valueString).matches()) {
        return i
      }
      iPatterns.length - 1
    }

    def calculatePrintedLength(value: Int): Int = {
      iSuffixes(selectSuffixIndex(value)).length
    }

    def printTo(buf: StringBuffer, value: Int) {
      buf.append(iSuffixes(selectSuffixIndex(value)))
    }

    def printTo(out: Writer, value: Int) {
      out.write(iSuffixes(selectSuffixIndex(value)))
    }

    def parse(periodStr: String, position: Int): Int = {
      for (text <- iSuffixesSortedDescByLength if periodStr.regionMatches(true, position, text, 0, text.length)
           if !matchesOtherAffix(text.length, periodStr, position)) {
        return position + text.length
      }
      ~position
    }

    def scan(periodStr: String, position: Int): Int = {
      val sourceLength = periodStr.length
      for (pos <- position until sourceLength; text <- iSuffixesSortedDescByLength if periodStr.regionMatches(true,
        pos, text, 0, text.length) if !matchesOtherAffix(text.length, periodStr, pos)) {
        return pos
      }
      ~position
    }

    def getAffixes(): Array[String] = iSuffixes.clone()
  }

  class CompositeAffix(private val iLeft: PeriodFieldAffix, private val iRight: PeriodFieldAffix)
    extends IgnorableAffix {

    val result = new HashSet[String]()
    private val iLeftRightCombinations = result.toArray(new Array[String](result.size))


    for (leftText <- iLeft.getAffixes; rightText <- iRight.getAffixes) {
      result.add(leftText + rightText)
    }

    def calculatePrintedLength(value: Int): Int = {
      iLeft.calculatePrintedLength(value) + iRight.calculatePrintedLength(value)
    }

    def printTo(buf: StringBuffer, value: Int) {
      iLeft.printTo(buf, value)
      iRight.printTo(buf, value)
    }

    def printTo(out: Writer, value: Int) {
      iLeft.printTo(out, value)
      iRight.printTo(out, value)
    }

    def parse(periodStr: String, position: Int): Int = {
      var pos = iLeft.parse(periodStr, position)
      if (pos >= 0) {
        pos = iRight.parse(periodStr, pos)
        if (pos >= 0 &&
          matchesOtherAffix(parse(periodStr, pos) - pos, periodStr, position)) {
          return ~position
        }
      }
      pos
    }

    def scan(periodStr: String, position: Int): Int = {
      val leftPosition = iLeft.scan(periodStr, position)
      if (leftPosition >= 0) {
        val rightPosition = iRight.scan(periodStr, iLeft.parse(periodStr, leftPosition))
        if (!(rightPosition >= 0 &&
          matchesOtherAffix(iRight.parse(periodStr, rightPosition) - leftPosition, periodStr, position))) {
          if (leftPosition > 0) {
            return leftPosition
          } else {
            return rightPosition
          }
        }
      }
      ~position
    }

    def getAffixes(): Array[String] = iLeftRightCombinations.clone()
  }

  class FieldFormatter extends PeriodPrinter with PeriodParser {

    private var iMinPrintedDigits: Int = _
    private var iPrintZeroSetting: Int = _
    private var iMaxParsedDigits: Int = _
    private var iRejectSignedValues: Boolean = _
    private var iFieldType: Int = _
    private var iFieldFormatters: Array[FieldFormatter] = null
    private var iPrefix: PeriodFieldAffix = null
    private var iSuffix: PeriodFieldAffix = null

    def this(minPrintedDigits: Int,
             printZeroSetting: Int,
             maxParsedDigits: Int,
             rejectSignedValues: Boolean,
             fieldType: Int,
             fieldFormatters: Array[FieldFormatter],
             prefix: PeriodFieldAffix,suffix: PeriodFieldAffix) = {
      this()
      iMinPrintedDigits = minPrintedDigits
      iPrintZeroSetting = printZeroSetting
      iMaxParsedDigits = maxParsedDigits
      iRejectSignedValues = rejectSignedValues
      iFieldType = fieldType
      iFieldFormatters = fieldFormatters
      iPrefix = prefix
      iSuffix = suffix
    }


    def this(field: FieldFormatter, suffix: PeriodFieldAffix) {
      this()
      var _suffix: PeriodFieldAffix = suffix
      iMinPrintedDigits = field.iMinPrintedDigits
      iPrintZeroSetting = field.iPrintZeroSetting
      iMaxParsedDigits = field.iMaxParsedDigits
      iRejectSignedValues = field.iRejectSignedValues
      iFieldType = field.iFieldType
      iFieldFormatters = field.iFieldFormatters
      iPrefix = field.iPrefix
      if (field.iSuffix != null) {
        _suffix = new CompositeAffix(field.iSuffix, _suffix)
      }
      iSuffix = _suffix
    }

    def finish(fieldFormatters: Array[FieldFormatter]) {
      val prefixesToIgnore = new HashSet[PeriodFieldAffix]()
      val suffixesToIgnore = new HashSet[PeriodFieldAffix]()
      for (fieldFormatter <- fieldFormatters if fieldFormatter != null && this != fieldFormatter) {
        prefixesToIgnore.add(fieldFormatter.iPrefix)
        suffixesToIgnore.add(fieldFormatter.iSuffix)
      }
      if (iPrefix != null) {
        iPrefix.finish(prefixesToIgnore)
      }
      if (iSuffix != null) {
        iSuffix.finish(suffixesToIgnore)
      }
    }

    def countFieldsToPrint(period: ReadablePeriod, stopAt: Int, locale: Locale): Int = {
      if (stopAt <= 0) {
        return 0
      }
      if (iPrintZeroSetting == PRINT_ZERO_ALWAYS || getFieldValue(period) != Long.MaxValue) {
        return 1
      }
      0
    }

    def calculatePrintedLength(period: ReadablePeriod, locale: Locale): Int = {
      var valueLong = getFieldValue(period)
      if (valueLong == Long.MaxValue) {
        return 0
      }
      var sum = Math.max(FormatUtils.calculateDigitCount(valueLong), iMinPrintedDigits)
      if (iFieldType >= SECONDS_MILLIS) {
        sum = (if (valueLong < 0) Math.max(sum, 5) else Math.max(sum, 4))
        sum += 1
        if (iFieldType == SECONDS_OPTIONAL_MILLIS &&
          (Math.abs(valueLong) % DateTimeConstants.MILLIS_PER_SECOND) ==
            0) {
          sum -= 4
        }
        valueLong = valueLong / DateTimeConstants.MILLIS_PER_SECOND
      }
      val value = valueLong.toInt
      if (iPrefix != null) {
        sum += iPrefix.calculatePrintedLength(value)
      }
      if (iSuffix != null) {
        sum += iSuffix.calculatePrintedLength(value)
      }
      sum
    }

    def printTo(buf: StringBuffer, period: ReadablePeriod, locale: Locale) {
      val valueLong = getFieldValue(period)
      if (valueLong == Long.MaxValue) {
        return
      }
      var value = valueLong.toInt
      if (iFieldType >= SECONDS_MILLIS) {
        value = (valueLong / DateTimeConstants.MILLIS_PER_SECOND).toInt
      }
      if (iPrefix != null) {
        iPrefix.printTo(buf, value)
      }
      val bufLen = buf.length
      val minDigits = iMinPrintedDigits
      if (minDigits <= 1) {
        FormatUtils.appendUnpaddedInteger(buf, value)
      } else {
        FormatUtils.appendPaddedInteger(buf, value, minDigits)
      }
      if (iFieldType >= SECONDS_MILLIS) {
        val dp = (Math.abs(valueLong) % DateTimeConstants.MILLIS_PER_SECOND).toInt
        if (iFieldType == SECONDS_MILLIS || dp > 0) {
          if (valueLong < 0 && valueLong > -DateTimeConstants.MILLIS_PER_SECOND) {
            buf.insert(bufLen, '-')
          }
          buf.append('.')
          FormatUtils.appendPaddedInteger(buf, dp, 3)
        }
      }
      if (iSuffix != null) {
        iSuffix.printTo(buf, value)
      }
    }

    def printTo(out: Writer, period: ReadablePeriod, locale: Locale) {
      val valueLong = getFieldValue(period)
      if (valueLong == Long.MaxValue) {
        return
      }
      var value = valueLong.toInt
      if (iFieldType >= SECONDS_MILLIS) {
        value = (valueLong / DateTimeConstants.MILLIS_PER_SECOND).toInt
      }
      if (iPrefix != null) {
        iPrefix.printTo(out, value)
      }
      val minDigits = iMinPrintedDigits
      if (minDigits <= 1) {
        FormatUtils.writeUnpaddedInteger(out, value)
      } else {
        FormatUtils.writePaddedInteger(out, value, minDigits)
      }
      if (iFieldType >= SECONDS_MILLIS) {
        val dp = (Math.abs(valueLong) % DateTimeConstants.MILLIS_PER_SECOND).toInt
        if (iFieldType == SECONDS_MILLIS || dp > 0) {
          out.write('.')
          FormatUtils.writePaddedInteger(out, dp, 3)
        }
      }
      if (iSuffix != null) {
        iSuffix.printTo(out, value)
      }
    }

    def parseInto(period: ReadWritablePeriod,
                  text: String,
                  position: Int,
                  locale: Locale): Int = {
      var _position: Int = position
      var mustParse = (iPrintZeroSetting == PRINT_ZERO_ALWAYS)
      if (_position >= text.length) {
        return if (mustParse) ~_position else _position
      }
      if (iPrefix != null) {
        _position = iPrefix.parse(text, _position)
        if (_position >= 0) {
          mustParse = true
        } else {
          if (!mustParse) {
            return ~_position
          }
          return _position
        }
      }
      var suffixPos = -1
      if (iSuffix != null && !mustParse) {
        suffixPos = iSuffix.scan(text, _position)
        if (suffixPos >= 0) {
          mustParse = true
        } else {
          if (!mustParse) {
            return ~suffixPos
          }
          return suffixPos
        }
      }
      if (!mustParse && !isSupported(period.getPeriodType, iFieldType)) {
        return _position
      }
      var limit: Int = 0
      limit = if (suffixPos > 0) Math.min(iMaxParsedDigits, suffixPos - _position) else Math.min(iMaxParsedDigits,
        text.length - _position)
      var length = 0
      var fractPos = -1
      var hasDigits = false

      var continueFlag = true
      while (length < limit) {
        continueFlag = true
        val c = text.charAt(_position + length)
        if (length == 0 && (c == '-' || c == '+') && !iRejectSignedValues) {
          val negative = c == '-'
          if (length + 1 >= limit || text.charAt(_position + length + 1) < '0' ||
            c > '9') {
            break()
          }
          if (negative) {
            length += 1
          } else {
            _position += 1
          }
          limit = Math.min(limit + 1, text.length - _position)
          continueFlag = false
        }
        if (continueFlag) {
          if (c >= '0' && c <= '9') {
            hasDigits = true
          } else {
            if ((c == '.' || c == ',') &&
              (iFieldType == SECONDS_MILLIS || iFieldType == SECONDS_OPTIONAL_MILLIS)) {
              if (fractPos >= 0) {
                break()
              }
              fractPos = _position + length + 1
              limit = Math.min(limit + 1, text.length - _position)
            } else {
              break()
            }
          }
          length += 1
        }
      }
      if (!hasDigits) {
        return ~_position
      }
      if (suffixPos >= 0 && _position + length != suffixPos) {
        return _position
      }
      if (iFieldType != SECONDS_MILLIS && iFieldType != SECONDS_OPTIONAL_MILLIS) {
        setFieldValue(period, iFieldType, parseInt(text, _position, length))
      } else if (fractPos < 0) {
        setFieldValue(period, SECONDS, parseInt(text, _position, length))
        setFieldValue(period, MILLIS, 0)
      } else {
        val wholeValue = parseInt(text, _position, fractPos - _position - 1)
        setFieldValue(period, SECONDS, wholeValue)
        val fractLen = _position + length - fractPos
        var fractValue: Int = 0
        if (fractLen <= 0) {
          fractValue = 0
        } else {
          if (fractLen >= 3) {
            fractValue = parseInt(text, fractPos, 3)
          } else {
            fractValue = parseInt(text, fractPos, fractLen)
            if (fractLen == 1) {
              fractValue *= 100
            } else {
              fractValue *= 10
            }
          }
          if (wholeValue < 0) {
            fractValue = -fractValue
          }
        }
        setFieldValue(period, MILLIS, fractValue)
      }
      _position += length
      if (_position >= 0 && iSuffix != null) {
        _position = iSuffix.parse(text, _position)
      }
      _position
    }

    private def parseInt(text: String, position: Int, length: Int): Int = {
      var _position: Int = position
      var _length: Int = length
      if (_length >= 10) {
        return Integer.parseInt(text.substring(_position, _position + _length))
      }
      if (_length <= 0) {
        return 0
      }
      _position += 1
      var value:Int = text.charAt(_position)
      _length -= 1
      var negative: Boolean = false
      if (value == '-') {
        if (_length < 0) {
          return 0
        }
        negative = true
        _position += 1
        value = text.charAt(_position)
      } else {
        negative = false
      }
      value -= '0'
      _length = _length -1
      while (_length > 0) {
        _position += 1
        value = (((value << 3) + (value << 1)) + text.charAt(_position) - '0').toChar
      }
      if (negative) -value else value
    }

    def getFieldValue(period: ReadablePeriod): Long = {
      var `type`: PeriodType = null
      `type` = if (iPrintZeroSetting == PRINT_ZERO_ALWAYS) null else period.getPeriodType
      if (`type` != null && isSupported(`type`, iFieldType) == false) {
        return Long.MaxValue
      }
      var value: Long = 0l
      iFieldType match {
        case YEARS => value = period.get(DurationFieldType.years())
        case MONTHS => value = period.get(DurationFieldType.months())
        case WEEKS => value = period.get(DurationFieldType.weeks())
        case DAYS => value = period.get(DurationFieldType.days())
        case HOURS => value = period.get(DurationFieldType.hours())
        case MINUTES => value = period.get(DurationFieldType.minutes())
        case SECONDS => value = period.get(DurationFieldType.seconds())
        case MILLIS => value = period.get(DurationFieldType.millis())
        case SECONDS_MILLIS | SECONDS_OPTIONAL_MILLIS =>
          val seconds = period.get(DurationFieldType.seconds())
          val millis = period.get(DurationFieldType.millis())
          value = (seconds * DateTimeConstants.MILLIS_PER_SECOND.toLong) +
            millis
        case _ => return Long.MaxValue

      }
      if (value == 0) iPrintZeroSetting match {
        case PRINT_ZERO_NEVER => return Long.MaxValue
        case PRINT_ZERO_RARELY_LAST => if (isZero(period) && iFieldFormatters(iFieldType) == this) {
          var i = iFieldType + 1
          while (i <= MAX_FIELD) {
            if (isSupported(`type`, i) && iFieldFormatters(i) != null) {
              return Long.MaxValue
            }
            i += 1
          }
        } else {
          return Long.MaxValue
        }
        case PRINT_ZERO_RARELY_FIRST => if (isZero(period) && iFieldFormatters(iFieldType) == this) {
          var i = Math.min(iFieldType, 8)
          i -= 1
          while (i >= 0 && i <= MAX_FIELD) {
            if (isSupported(`type`, i) && iFieldFormatters(i) != null) {
              return Long.MaxValue
            }
            i -= 1
          }
        } else {
          return Long.MaxValue
        }
      }
      value
    }

    def isZero(period: ReadablePeriod): Boolean = {
      (0 until period.size()).find(period.getValue(_) != 0).map(_ => false)
        .getOrElse(true)
    }

    def isSupported(`type`: PeriodType, field: Int): Boolean = field match {
      case YEARS => `type`.isSupported(DurationFieldType.years())
      case MONTHS => `type`.isSupported(DurationFieldType.months())
      case WEEKS => `type`.isSupported(DurationFieldType.weeks())
      case DAYS => `type`.isSupported(DurationFieldType.days())
      case HOURS => `type`.isSupported(DurationFieldType.hours())
      case MINUTES => `type`.isSupported(DurationFieldType.minutes())
      case SECONDS => `type`.isSupported(DurationFieldType.seconds())
      case MILLIS => `type`.isSupported(DurationFieldType.millis())
      case SECONDS_MILLIS | SECONDS_OPTIONAL_MILLIS => `type`.isSupported(DurationFieldType.seconds()) || `type`.isSupported(DurationFieldType.millis())
      case _ => false
    }

    def setFieldValue(period: ReadWritablePeriod, field: Int, value: Int):Unit = field match {
      case YEARS => period.setYears(value)
      case MONTHS => period.setMonths(value)
      case WEEKS => period.setWeeks(value)
      case DAYS => period.setDays(value)
      case HOURS => period.setHours(value)
      case MINUTES => period.setMinutes(value)
      case SECONDS => period.setSeconds(value)
      case MILLIS => period.setMillis(value)
      case _ =>
    }

    def getFieldType(): Int = iFieldType
  }

  object Literal {

    val EMPTY = new Literal("")
  }

  class Literal(private val iText: String) extends PeriodPrinter with PeriodParser {

    def countFieldsToPrint(period: ReadablePeriod, stopAt: Int, locale: Locale): Int = 0

    def calculatePrintedLength(period: ReadablePeriod, locale: Locale): Int = iText.length

    def printTo(buf: StringBuffer, period: ReadablePeriod, locale: Locale) {
      buf.append(iText)
    }

    def printTo(out: Writer, period: ReadablePeriod, locale: Locale) {
      out.write(iText)
    }

    def parseInto(period: ReadWritablePeriod,
                  periodStr: String,
                  position: Int,
                  locale: Locale): Int = {
      if (periodStr.regionMatches(true, position, iText, 0, iText.length)) {
        return position + iText.length
      }
      ~position
    }
  }

  class Separator(private val text: String,
                  private val finalText: String,
                  variants: Array[String],
                  private val beforePrinter: PeriodPrinter,
                  private val beforeParser: PeriodParser,
                  private val useBefore: Boolean,
                  private val useAfter: Boolean) extends PeriodPrinter with PeriodParser {

    private var iParsedForms: Array[String] = null
    @volatile var iAfterPrinter: PeriodPrinter = null
    @volatile var iAfterParser: PeriodParser = null
    private var iFinalText: String = null
    private var iText: String = null
    private var iBeforePrinter: PeriodPrinter = null
    private var iBeforeParser: PeriodParser = null
    private var iUseBefore: Boolean = _
    private var iUseAfter: Boolean = _

    iText = text
    iFinalText = finalText

    if ((finalText == null || text == finalText) && (variants == null || variants.length == 0)) {
      iParsedForms = Array(text)
    } else {
      val parsedSet = new TreeSet[String](String.CASE_INSENSITIVE_ORDER)
      parsedSet.add(text)
      parsedSet.add(finalText)
      if (variants != null) {
        val i = variants.length
        while (i >= 0) {
          parsedSet.add(variants(i))
        }
      }
      val parsedList = new ArrayList[String](parsedSet)
      Collections.reverse(parsedList)
      iParsedForms = parsedList.toArray(Array.ofDim[String](parsedList.size))
    }

    iBeforePrinter = beforePrinter
    iBeforeParser = beforeParser
    iUseBefore = useBefore
    iUseAfter = useAfter

    def countFieldsToPrint(period: ReadablePeriod, stopAt: Int, locale: Locale): Int = {
      var sum = iBeforePrinter.countFieldsToPrint(period, stopAt, locale)
      if (sum < stopAt) {
        sum += iAfterPrinter.countFieldsToPrint(period, stopAt, locale)
      }
      sum
    }

    def calculatePrintedLength(period: ReadablePeriod, locale: Locale): Int = {
      val before = iBeforePrinter
      val after = iAfterPrinter
      var sum = before.calculatePrintedLength(period, locale) + after.calculatePrintedLength(period,
        locale)
      if (iUseBefore) {
        if (before.countFieldsToPrint(period, 1, locale) > 0) {
          if (iUseAfter) {
            val afterCount = after.countFieldsToPrint(period, 2, locale)
            if (afterCount > 0) {
              sum += (if (afterCount > 1) iText else iFinalText).length
            }
          } else {
            sum += iText.length
          }
        }
      } else if (iUseAfter && after.countFieldsToPrint(period, 1, locale) > 0) {
        sum += iText.length
      }
      sum
    }

    def printTo(buf: StringBuffer, period: ReadablePeriod, locale: Locale) {
      val before = iBeforePrinter
      val after = iAfterPrinter
      before.printTo(buf, period, locale)
      if (iUseBefore) {
        if (before.countFieldsToPrint(period, 1, locale) > 0) {
          if (iUseAfter) {
            val afterCount = after.countFieldsToPrint(period, 2, locale)
            if (afterCount > 0) {
              buf.append(if (afterCount > 1) iText else iFinalText)
            }
          } else {
            buf.append(iText)
          }
        }
      } else if (iUseAfter && after.countFieldsToPrint(period, 1, locale) > 0) {
        buf.append(iText)
      }
      after.printTo(buf, period, locale)
    }

    def printTo(out: Writer, period: ReadablePeriod, locale: Locale) {
      val before = iBeforePrinter
      val after = iAfterPrinter
      before.printTo(out, period, locale)
      if (iUseBefore) {
        if (before.countFieldsToPrint(period, 1, locale) > 0) {
          if (iUseAfter) {
            val afterCount = after.countFieldsToPrint(period, 2, locale)
            if (afterCount > 0) {
              out.write(if (afterCount > 1) iText else iFinalText)
            }
          } else {
            out.write(iText)
          }
        }
      } else if (iUseAfter && after.countFieldsToPrint(period, 1, locale) > 0) {
        out.write(iText)
      }
      after.printTo(out, period, locale)
    }

    def parseInto(period: ReadWritablePeriod,
                  periodStr: String,
                  position: Int,
                  locale: Locale): Int = {
      var _position: Int = position
      var oldPos = _position
      _position = iBeforeParser.parseInto(period, periodStr, _position, locale)
      if (_position < 0) {
        return _position
      }
      var found = false
      var parsedFormLength = -1
      if (_position > oldPos) {
        val parsedForms = iParsedForms
        val length = parsedForms.length
        for (i <- 0 until length) {
          val parsedForm = parsedForms(i)
          if ((parsedForm == null || parsedForm.length == 0) ||
            periodStr.regionMatches(true, _position, parsedForm, 0, parsedForm.length)) {
            parsedFormLength = if (parsedForm == null) 0 else parsedForm.length
            _position += parsedFormLength
            found = true
            break()
          }
        }
      }
      oldPos = _position
      _position = iAfterParser.parseInto(period, periodStr, _position, locale)
      if (_position < 0) {
        return _position
      }
      if (found && _position == oldPos && parsedFormLength > 0) {
        return ~oldPos
      }
      if (_position > oldPos && !found && !iUseBefore) {
        return ~oldPos
      }
      _position
    }

    def finish(afterPrinter: PeriodPrinter, afterParser: PeriodParser): Separator = {
      iAfterPrinter = afterPrinter
      iAfterParser = afterParser
      this
    }
  }

  class Composite(elementPairs: List[Any]) extends PeriodPrinter with PeriodParser {

    val printerList = new ArrayList[Any]()

    val parserList = new ArrayList[Any]()

    private val iPrinters = if (printerList.size <= 0) null else printerList.toArray(new Array[PeriodPrinter](printerList.size))

    private val iParsers = if (parserList.size <= 0) null else parserList.toArray(new Array[PeriodParser](parserList.size))

    decompose(elementPairs, printerList, parserList)

    def countFieldsToPrint(period: ReadablePeriod, stopAt: Int, locale: Locale): Int = {
      var sum = 0
      val printers = iPrinters
      val i = printers.length
      while (sum < stopAt && i >= 0) {
        sum += printers(i).countFieldsToPrint(period, Integer.MAX_VALUE, locale)
      }
      sum
    }

    def calculatePrintedLength(period: ReadablePeriod, locale: Locale): Int = {
      var sum = 0
      val printers = iPrinters
      val i = printers.length
      while (i >= 0) {
        sum += printers(i).calculatePrintedLength(period, locale)
      }
      sum
    }

    def printTo(buf: StringBuffer, period: ReadablePeriod, locale: Locale) {
      val printers = iPrinters
      val len = printers.length
      for (i <- 0 until len) {
        printers(i).printTo(buf, period, locale)
      }
    }

    def printTo(out: Writer, period: ReadablePeriod, locale: Locale) {
      val printers = iPrinters
      val len = printers.length
      for (i <- 0 until len) {
        printers(i).printTo(out, period, locale)
      }
    }

    def parseInto(period: ReadWritablePeriod,
                  periodStr: String,
                  position: Int,
                  locale: Locale): Int = {
      var _position: Int = position
      val parsers = iParsers
      if (parsers == null) {
        throw new UnsupportedOperationException()
      }
      val len = parsers.length
      var i = 0
      while (i < len && _position >= 0) {
        _position = parsers(i).parseInto(period, periodStr, _position, locale)
        i += 1
      }
      _position
    }

    private def decompose(elementPairs: List[Any], printerList: List[Any], parserList: List[Any]) {
      val size = elementPairs.size
      var i = 0
      while (i < size) {
        var element = elementPairs.get(i)
        if (element.isInstanceOf[PeriodPrinter]) {
          if (element.isInstanceOf[Composite]) {
            addArrayToList(printerList, element.asInstanceOf[Composite].iPrinters.map(_.asInstanceOf[Any]))
          } else {
            printerList.add(element)
          }
        }
        element = elementPairs.get(i + 1)
        if (element.isInstanceOf[PeriodParser]) {
          if (element.isInstanceOf[Composite]) {
            addArrayToList(parserList, element.asInstanceOf[Composite].iPrinters.map(_.asInstanceOf[Any]))
          } else {
            parserList.add(element)
          }
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
}

class PeriodFormatterBuilder {

  private var iMinPrintedDigits: Int = _
  private var iPrintZeroSetting: Int = _
  private var iMaxParsedDigits: Int = _
  private var iRejectSignedValues: Boolean = _
  private var iPrefix: PeriodFieldAffix = null
  private var iElementPairs: List[Any] = null
  private var iNotPrinter: Boolean = _
  private var iNotParser: Boolean = _
  private var iFieldFormatters: Array[FieldFormatter] = null

  clear()

  def toFormatter(): PeriodFormatter = {
    val formatter = PeriodFormatterBuilder.toFormatter(iElementPairs, iNotPrinter, iNotParser)
    for (fieldFormatter <- iFieldFormatters if fieldFormatter != null) {
      fieldFormatter.finish(iFieldFormatters)
    }
    iFieldFormatters = iFieldFormatters.clone().asInstanceOf[Array[FieldFormatter]]
    formatter
  }

  def toPrinter(): PeriodPrinter = {
    if (iNotPrinter) {
      return null
    }
    toFormatter().getPrinter
  }

  def toParser(): PeriodParser = {
    if (iNotParser) {
      return null
    }
    toFormatter().getParser
  }

  def clear() {
    iMinPrintedDigits = 1
    iPrintZeroSetting = PRINT_ZERO_RARELY_LAST
    iMaxParsedDigits = 10
    iRejectSignedValues = false
    iPrefix = null
    if (iElementPairs == null) {
      iElementPairs = new ArrayList[Any]()
    } else {
      iElementPairs.clear()
    }
    iNotPrinter = false
    iNotParser = false
    iFieldFormatters = Array.ofDim[FieldFormatter](10)
  }

  def append(formatter: PeriodFormatter): PeriodFormatterBuilder = {
    if (formatter == null) {
      throw new IllegalArgumentException("No formatter supplied")
    }
    clearPrefix()
    append0(formatter.getPrinter, formatter.getParser)
    this
  }

  def append(printer: PeriodPrinter, parser: PeriodParser): PeriodFormatterBuilder = {
    if (printer == null && parser == null) {
      throw new IllegalArgumentException("No printer or parser supplied")
    }
    clearPrefix()
    append0(printer, parser)
    this
  }

  def appendLiteral(text: String): PeriodFormatterBuilder = {
    if (text == null) {
      throw new IllegalArgumentException("Literal must not be null")
    }
    clearPrefix()
    val literal = new Literal(text)
    append0(literal, literal)
    this
  }

  def minimumPrintedDigits(minDigits: Int): PeriodFormatterBuilder = {
    iMinPrintedDigits = minDigits
    this
  }

  def maximumParsedDigits(maxDigits: Int): PeriodFormatterBuilder = {
    iMaxParsedDigits = maxDigits
    this
  }

  def rejectSignedValues(v: Boolean): PeriodFormatterBuilder = {
    iRejectSignedValues = v
    this
  }

  def printZeroRarelyLast(): PeriodFormatterBuilder = {
    iPrintZeroSetting = PRINT_ZERO_RARELY_LAST
    this
  }

  def printZeroRarelyFirst(): PeriodFormatterBuilder = {
    iPrintZeroSetting = PRINT_ZERO_RARELY_FIRST
    this
  }

  def printZeroIfSupported(): PeriodFormatterBuilder = {
    iPrintZeroSetting = PRINT_ZERO_IF_SUPPORTED
    this
  }

  def printZeroAlways(): PeriodFormatterBuilder = {
    iPrintZeroSetting = PRINT_ZERO_ALWAYS
    this
  }

  def printZeroNever(): PeriodFormatterBuilder = {
    iPrintZeroSetting = PRINT_ZERO_NEVER
    this
  }

  def appendPrefix(text: String): PeriodFormatterBuilder = {
    if (text == null) {
      throw new IllegalArgumentException()
    }
    appendPrefix(new SimpleAffix(text))
  }

  def appendPrefix(singularText: String, pluralText: String): PeriodFormatterBuilder = {
    if (singularText == null || pluralText == null) {
      throw new IllegalArgumentException()
    }
    appendPrefix(new PluralAffix(singularText, pluralText))
  }

  def appendPrefix(regularExpressions: Array[String], prefixes: Array[String]): PeriodFormatterBuilder = {
    if (regularExpressions == null || prefixes == null || regularExpressions.length < 1 ||
      regularExpressions.length != prefixes.length) {
      throw new IllegalArgumentException()
    }
    appendPrefix(new RegExAffix(regularExpressions, prefixes))
  }

  private def appendPrefix(prefix: PeriodFieldAffix): PeriodFormatterBuilder = {
    var _prefix: PeriodFieldAffix = prefix
    if (_prefix == null) {
      throw new IllegalArgumentException()
    }
    if (iPrefix != null) {
      _prefix = new CompositeAffix(iPrefix, _prefix)
    }
    iPrefix = _prefix
    this
  }

  def appendYears(): PeriodFormatterBuilder = {
    appendField(YEARS)
    this
  }

  def appendMonths(): PeriodFormatterBuilder = {
    appendField(MONTHS)
    this
  }

  def appendWeeks(): PeriodFormatterBuilder = {
    appendField(WEEKS)
    this
  }

  def appendDays(): PeriodFormatterBuilder = {
    appendField(DAYS)
    this
  }

  def appendHours(): PeriodFormatterBuilder = {
    appendField(HOURS)
    this
  }

  def appendMinutes(): PeriodFormatterBuilder = {
    appendField(MINUTES)
    this
  }

  def appendSeconds(): PeriodFormatterBuilder = {
    appendField(SECONDS)
    this
  }

  def appendSecondsWithMillis(): PeriodFormatterBuilder = {
    appendField(SECONDS_MILLIS)
    this
  }

  def appendSecondsWithOptionalMillis(): PeriodFormatterBuilder = {
    appendField(SECONDS_OPTIONAL_MILLIS)
    this
  }

  def appendMillis(): PeriodFormatterBuilder = {
    appendField(MILLIS)
    this
  }

  def appendMillis3Digit(): PeriodFormatterBuilder = {
    appendField(7, 3)
    this
  }

  private def appendField(`type`: Int) {
    appendField(`type`, iMinPrintedDigits)
  }

  private def appendField(`type`: Int, minPrinted: Int) {
    val field = new FieldFormatter(minPrinted, iPrintZeroSetting, iMaxParsedDigits, iRejectSignedValues,
      `type`, iFieldFormatters, iPrefix, null)
    append0(field, field)
    iFieldFormatters(`type`) = field
    iPrefix = null
  }

  def appendSuffix(text: String): PeriodFormatterBuilder = {
    if (text == null) {
      throw new IllegalArgumentException()
    }
    appendSuffix(new SimpleAffix(text))
  }

  def appendSuffix(singularText: String, pluralText: String): PeriodFormatterBuilder = {
    if (singularText == null || pluralText == null) {
      throw new IllegalArgumentException()
    }
    appendSuffix(new PluralAffix(singularText, pluralText))
  }

  def appendSuffix(regularExpressions: Array[String], suffixes: Array[String]): PeriodFormatterBuilder = {
    if (regularExpressions == null || suffixes == null || regularExpressions.length < 1 ||
      regularExpressions.length != suffixes.length) {
      throw new IllegalArgumentException()
    }
    appendSuffix(new RegExAffix(regularExpressions, suffixes))
  }

  private def appendSuffix(suffix: PeriodFieldAffix): PeriodFormatterBuilder = {
    var originalPrinter: Any = null
    var originalParser: Any = null
    if (iElementPairs.size > 0) {
      originalPrinter = iElementPairs.get(iElementPairs.size - 2)
      originalParser = iElementPairs.get(iElementPairs.size - 1)
    } else {
      originalPrinter = null
      originalParser = null
    }
    if (originalPrinter == null || originalParser == null || originalPrinter != originalParser ||
      !originalPrinter.isInstanceOf[FieldFormatter]) {
      throw new IllegalStateException("No field to apply suffix to")
    }
    clearPrefix()
    val newField = new FieldFormatter(originalPrinter.asInstanceOf[FieldFormatter], suffix)
    iElementPairs.set(iElementPairs.size - 2, newField)
    iElementPairs.set(iElementPairs.size - 1, newField)
    iFieldFormatters(newField.getFieldType) = newField
    this
  }

  def appendSeparator(text: String): PeriodFormatterBuilder = {
    appendSeparator(text, text, null, true, true)
  }

  def appendSeparatorIfFieldsAfter(text: String): PeriodFormatterBuilder = {
    appendSeparator(text, text, null, false, true)
  }

  def appendSeparatorIfFieldsBefore(text: String): PeriodFormatterBuilder = {
    appendSeparator(text, text, null, true, false)
  }

  def appendSeparator(text: String, finalText: String): PeriodFormatterBuilder = {
    appendSeparator(text, finalText, null, true, true)
  }

  def appendSeparator(text: String, finalText: String, variants: Array[String]): PeriodFormatterBuilder = {
    appendSeparator(text, finalText, variants, true, true)
  }

  private def appendSeparator(text: String,
                              finalText: String,
                              variants: Array[String],
                              useBefore: Boolean,
                              useAfter: Boolean): PeriodFormatterBuilder = {
    if (text == null || finalText == null) {
      throw new IllegalArgumentException()
    }
    clearPrefix()
    var pairs = iElementPairs
    if (pairs.size == 0) {
      if (useAfter && useBefore == false) {
        val separator = new Separator(text, finalText, variants, Literal.EMPTY, Literal.EMPTY, useBefore,
          useAfter)
        append0(separator, separator)
      }
      return this
    }
    var i: Int = 0
    var lastSeparator: Separator = null
    i = pairs.size
    while (i >= 0) {
      if (pairs.get(i).isInstanceOf[Separator]) {
        lastSeparator = pairs.get(i).asInstanceOf[Separator]
        pairs = pairs.subList(i + 1, pairs.size)
        break()
      }
      i -= 1
    }
    if (lastSeparator != null && pairs.size == 0) {
      throw new IllegalStateException("Cannot have two adjacent separators")
    } else {
      val comp = createComposite(pairs)
      pairs.clear()
      val separator = new Separator(text, finalText, variants, comp(0).asInstanceOf[PeriodPrinter], comp(1).asInstanceOf[PeriodParser],
        useBefore, useAfter)
      pairs.add(separator)
      pairs.add(separator)
    }
    this
  }

  private def clearPrefix() {
    if (iPrefix != null) {
      throw new IllegalStateException("Prefix not followed by field")
    }
    iPrefix = null
  }

  private def append0(printer: PeriodPrinter, parser: PeriodParser): PeriodFormatterBuilder = {
    iElementPairs.add(printer)
    iElementPairs.add(parser)
    iNotPrinter |= (printer == null)
    iNotParser |= (parser == null)
    this
  }
}
